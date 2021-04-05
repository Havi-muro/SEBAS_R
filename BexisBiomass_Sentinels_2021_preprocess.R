################################################################

# Preprocess the bexis data and run and Random Forest Regression
# to predict biomass and biodiversity data

# Field data from bexis Botany core team
# S2 data is from single images taken as close as possible to biomass collection date
# and from qualityMosaic() composites. these composites have some errors
# but it is a much more convinient way of retrieving the spectral bands

# Predictors: LUI, slope, aspect, soil type and satellite info
# Sensors used: Sentinel-2 vegetation indices for may and Sentinel-
# annual metrics

# It estimates mowing dates from the interviews with farmers

################################################################

library(readxl)
library(readr) 
library(data.table)
library(GGally)
library(lattice)  
library(ggplot2)
library(ggradar)
library(mgcv)
library(lme4)
library(rgdal)
library(sp)
library(gstat)
library(raster)
library(plyr)
library(reshape)
library(fields)
library(mapdata)
library(tidyr)
library(caret)
library(robustHD)
library(randomForest)


#############################################################################################################
######################################    DATA EXPLORATION    ###############################################
#############################################################################################################

setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model')
dir()

#Open different datasets

#Biomass data and S2 values from single images
BexisRaw <- read_excel("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4.xlsx", sheet = 'Sheet1')

#cutting dates
BexisSurvey <- fread('Land use in grasslands_ raw data of yearly owner interviews-Filtered.txt')
BexisSurvey <- as.data.frame(BexisSurvey)

str(BexisSurvey)
str(BexisRaw)

# Get the Sentinel-2 data from qualityMosaic() composites
setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/S2_metrics')

#Read S2 metrics and bind them
ALB2017<- fread('ALB_2017_S2_QMNDVI.csv')
ALB2018<- fread('ALB_2018_S2_QMNDVI.csv')
ALB2019<- fread('ALB_2019_S2_QMNDVI.csv')
ALB2020<- fread('ALB_2020_S2_QMNDVI.csv')

HAI2017<- fread('HAI_2017_S2_QMNDVI.csv')
HAI2018<- fread('HAI_2018_S2_QMNDVI.csv')
HAI2019<- fread('HAI_2019_S2_QMNDVI.csv')
HAI2020<- fread('HAI_2020_S2_QMNDVI.csv')

SCH2017<- fread('SCH_2017_S2_QMNDVI.csv')
SCH2018<- fread('SCH_2018_S2_QMNDVI.csv')
SCH2019<- fread('SCH_2019_S2_QMNDVI.csv')
SCH2020<- fread('SCH_2020_S2_QMNDVI.csv')

S2Metrics <- rbind(ALB2017, ALB2018, ALB2019, ALB2020, HAI2017, HAI2018, HAI2019, HAI2020, SCH2017, SCH2018, SCH2019, SCH2020)

#Select mowing data variables and transform date to DOY
CutDate_vars <- c('Year', 'EP_PlotID' , 'DateCut1', 'Cuts')
CutDate <- BexisSurvey[CutDate_vars]
str(CutDate)

tmp <- as.POSIXlt(CutDate$DateCut1, format = "%Y-%m-%d")
tmp$yday
CutDOY <- cbind(CutDate, tmp$yday)
str(CutDOY)

#change names of common fields and merge biomass & cutting dates
names(CutDOY)[names(CutDOY) == "EP_PlotID"] <- "EpPlotID"
names(CutDOY)[names(CutDOY) == "tmp$yday"] <- "CutDOY"

str(CutDOY)

#Merge by several variables
BexisMerge <- merge(BexisRaw, CutDOY, by=c('Year', 'EpPlotID'), all.x=TRUE)
str(BexisMerge)

BexisRecent <- subset(BexisMerge, Year > 2016)

#Plot the cut dates box plots
p <- ggplot(data = BexisRecent,
            aes( x = explo, 
                 y = CutDOY,
                 ))
p <- p + geom_boxplot()   
p

#Plot the number of cuts per exploratory
c <- ggplot(data = BexisRecent,
            aes( x = Cuts,
                 ))
c <- c + geom_bar()
c <- c + facet_grid(rows = vars(explo), scales = "fixed")
c

#Plot the dates of biomass collection
d <- ggplot(data = BexisRecent,
            aes( x = explo, 
                 y = as.numeric(DOY_bm),
            ))
d <- d + geom_boxplot()  
d

##################################################################################
#Calculate stats of the mowing and biomass dates for recent dates
tapply(as.numeric(BexisRecent$DOY_bm), BexisRecent$explo, summary)
tapply(BexisRecent$CutDOY, BexisRecent$explo, summary)

#ALB: mowing is around 21 June
#HAI: mowing is around 12 June
#SCH: mowing is around 28 June
#Biomass collection in Alb is between 12 & 17 May
#Biomass collection in Hai is between 11 & 18 May
#Biomass collection in SCH is between 19 & 25 May

##################################################################################

# Merge bexis data taht contains single image Sentinel-2 data with satellite info
# from qualityMoisaic composites
Bexis_S2Metrics <- merge(BexisRecent, S2Metrics, by=c('Year','ep'))
str(Bexis_S2Metrics)
variable.names(Bexis_S2Metrics)

# Plot bands from mosaic and image to see dissimilarities
# plot(Bexis_S2Metrics$S2b2, Bexis_S2Metrics$B2)
Bexis_S2Metrics_SWSubs <- subset(Bexis_S2Metrics, SW == 2)
plot(Bexis_S2Metrics_SWSubs$S2b8, Bexis_S2Metrics_SWSubs$B8, xlab = "S2 B8 Single image", ylab = "S2 B8 qualityMosaic()")
plot(Bexis_S2Metrics_SWSubs$S2b12, Bexis_S2Metrics_SWSubs$B12, xlab = "S2 B12 Single image", ylab = "S2 B12 qualityMosaic()")

Bexis_S2Metrics_SWSubs2 <- subset(Bexis_S2Metrics, B3 > 300 & S2b3 < 2000)



#Select the Final Variables we will use
MyVars <- c("Year", "ep", 'explo', "x","y", 'SW',"number_vascular_plants","biomass_g", 
            "SpecRichness", "Shannon", "Simpson", "FisherAlpha", "PielouEvenness",
            "LUI_2015_2018", "SoilTypeFusion","slope" ,"aspect", 
            'S2b2','S2b3','S2b4','S2b5','S2b6','S2b7','S2b8','S2b8a','S2b11','S2b12', 'LAI',
            'B2','B3', 'B4', 'B5', 'B6', 'B7','B8','B8A', 'B11', 'B12',
            'VHMean_May', 'VVMean_May','NDVI.y'    
            )

BexisFV<-Bexis_S2Metrics_SWSubs[MyVars]
str(BexisFV)

# NAs cause R to read some field as string. Transform to numeric
BexisFVnum <- transform(BexisFV, number_vascular_plants = as.numeric(number_vascular_plants), 
                        SpecRichness = as.numeric(SpecRichness),
                        Shannon  = as.numeric(Shannon),
                        Simpson = as.numeric(Simpson),
                        FisherAlpha = as.numeric(FisherAlpha),
                        PielouEvenness = as.numeric(PielouEvenness)
                        )
str(BexisFVnum)

# Calculate several vegetation indices (https://www.indexdatabase.de/db/s-single.php?id=96)
BexisFVnum$EVI <- cbind(2.5*(BexisFVnum$B8-BexisFVnum$B4)/((BexisFVnum$B8+6*BexisFVnum$B4-7.5*BexisFVnum$B2)+1))
BexisFVnum$SAVI <- cbind((BexisFVnum$B8-BexisFVnum$B4)/(BexisFVnum$B8+BexisFVnum$B4+0.428)*(1+0.428))
BexisFVnum$GNDVI <- cbind((BexisFVnum$B8-BexisFVnum$B3)/(BexisFVnum$B8+BexisFVnum$B3))
BexisFVnum$ARVI <- cbind((BexisFVnum$B8A-BexisFVnum$B4-0.106*(BexisFVnum$B4-BexisFVnum$B2))/(BexisFVnum$B8A+BexisFVnum$B4-0.106*(BexisFVnum$B4-BexisFVnum$B2)))
BexisFVnum$CHLRE <- cbind((BexisFVnum$B7/BexisFVnum$B5)^(-1))
BexisFVnum$MCARI <- cbind((BexisFVnum$B5-BexisFVnum$B4)-0.2*(BexisFVnum$B5-BexisFVnum$B3)*(BexisFVnum$B5/BexisFVnum$B4))
BexisFVnum$NDII <- cbind((BexisFVnum$B8-BexisFVnum$B11)/(BexisFVnum$B8+BexisFVnum$B11))
BexisFVnum$MIRNIR <- cbind((BexisFVnum$B12-BexisFVnum$B8)/(BexisFVnum$B12+BexisFVnum$B8))
BexisFVnum$MNDVI <- cbind((BexisFVnum$B8A-BexisFVnum$B12)/(BexisFVnum$B8A+BexisFVnum$B12))

MyVars2 <- c("Year", "ep", 'explo', "x","y","number_vascular_plants","biomass_g", 
            "SpecRichness", "Shannon", "Simpson", "FisherAlpha", "PielouEvenness",
            "LUI_2015_2018", "SoilTypeFusion","slope" ,"aspect", 
            'S2b2','S2b3','S2b4','S2b5','S2b6','S2b7','S2b8','S2b8a','S2b11','S2b12', 'LAI',
            'B2','B3', 'B4', 'B5', 'B6', 'B7','B8','B8A', 'B11', 'B12',
            'EVI','SAVI', 'GNDVI', 'ARVI', 'CHLRE', 'MCARI','NDII','MIRNIR', 'MNDVI',
            'VHMean_May', 'VVMean_May','NDVI.y'  
            )

BexisIndices<-BexisFVnum[MyVars2]
str(BexisIndices)

write.csv(BexisIndices, file = 'BexisS2Img_and_MaxNDVIComposite.csv')

##################  BIOMASS ########################

#Are there NAs in biomass?
is.na(BexisIndices$biomass_g)   
sum(is.na(BexisIndices$biomass_g))
colSums(is.na(BexisIndices))

#Are there NAs in NDVI.y?
is.na(BexisIndices$NDVI.y)   
sum(is.na(BexisIndices$NDVI.y))
colSums(is.na(BexisIndices))

# Remove missing values:
BexisIndices2 <- subset(BexisIndices, biomass_g != "NA")
BexisIndices3 <- subset(BexisIndices2, NDVI.y != "NA")

#Plot to discover outliers and remove them
plot(BexisIndices3$biomass_g)
plot(BexisIndices3$NDVI.y)
#identify(x=Bexis7$S2b3)

#Export for DL comparison
#write.csv(Bexis7, "AGB_Biodiv_bexis_forRF.csv")

#Post-hoc testing, to see if there are observer, year or location effect.
p <- ggplot(data = BexisIndices3,
             aes(	x = NDVI.y, 
                  y = biomass_g,
                  col = explo, 
                  group = explo,))
p <- p + geom_point()
p <- p + xlab("NDVI") + ylab("biomass")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(data = BexisIndices3, 
            method = "lm", 
            se = FALSE, 
            aes(y = biomass_g, 
                x = NDVI.y, 
                group = explo,  
                col = explo))  
p <- p + facet_grid(Year ~ explo, scales = "fixed")
p


#Conclusions of data exploration
#Points in SCH seem to have a much larger variance in biomass

#############################################################################################################
####################################      Radar Plot       ##################################################
#############################################################################################################

# Eliminate all records with NAs
Bexis_noNAN<-na.omit(BexisIndices)

# Select the variables for the radar plot
MyVars_STD <- c("biomass_g", 'LUI_2015_2018',
             "slope" ,"aspect"
             #'EVI','SAVI',
             #'GNDVI', 'ARVI', 'CHLRE', 'MCARI','NDII','MIRNIR','MNDVI',
             #'VHMax_May', 'VVMax_May','NDVI.x', 'VVStd', 'VHStd'  
)


# Separate observation info, soils and variables to standardize
MyVarsInfo <- c("Year","ep", "explo")

Bexis_info<-Bexis_noNAN[MyVarsInfo]

# Separate soil types and One hot encode them
soil <- Bexis_noNAN['SoilTypeFusion']
Bexis_hotSoil <- dummyVars(" ~ .", data = soil)
Bexis_hotSoil2 <- data.frame(predict(Bexis_hotSoil, newdata = soil))

names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionBraunerde"] <- "Braunerde"
names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionErdniedermoor"] <- "Erdniedermoor"
names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionFahlerde"] <- "Fahlerde"
names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionMulmniedermoor"] <- "Mulmniedermoor"
names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionParabraunerde"] <- "Parabraunerde"
names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionPseudogley"] <- "Pseudogley"
names(Bexis_hotSoil2)[names(Bexis_hotSoil2) == "SoilTypeFusionRendzina"] <- "Rendzina"

head(Bexis_hotSoil2)

# Separate the numerical variables and standardize them
Bexis_STD<-Bexis_noNAN[MyVars_STD]

Bexis_STD2<-robStandardize(Bexis_STD)
head(Bexis_STD2)

# bind the obs info, the one hot encoded soils and the standardized numerical variables
BexisRada <- cbind(Bexis_info, Bexis_STD2, Bexis_hotSoil2)
head(BexisRada)

# The radar plot is built with mean data of each variable in each site and year
# Select site and year. Radar plot is produced in excel.
# Perhaps I have to standardize by year?
explo1 <- 'ALB'
#Year1 <- '2020'

siteyear<-subset(BexisRada, explo == explo1# & Year == Year1
                 )
head(siteyear)
vars_num<-siteyear[, 4:28]
vars_mean <- colMeans(vars_num)
vars_stdev <- apply(BexisRada,2,sd)
write.csv(vars_stdev, file = 'Stdev_radarPlot.csv')
setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/RadarPlot')
write.csv(vars_mean, file = paste(explo1,'total', 
                                  #Year1, 
                                  '.csv'))
