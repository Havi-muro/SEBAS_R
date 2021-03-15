################################################################

# Preprocess the bexis data and run and Random Forest Regression
# to predict biomass and biodiversity data

# Field data from bexis Botany core team

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

#Biomass data
BexisRaw <- read_excel("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4.xlsx", sheet = 'Sheet1')

#cutting dates
BexisSurvey <- fread('Land use in grasslands_ raw data of yearly owner interviews-Filtered.txt')
BexisSurvey <- as.data.frame(BexisSurvey)

str(BexisSurvey)
str(BexisRaw)

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
#Merge bexis data with satellite info
Bexis_S2Metrics <- merge(BexisRecent, S2Metrics, by=c('Year','ep'))
str(Bexis_S2Metrics)
variable.names(Bexis_S2Metrics)
#Select the Final Variables we will use
MyVars <- c("Year", "ep", 'explo', "x","y","number_vascular_plants","biomass_g", 
            "SpecRichness", "Shannon", "Simpson", "FisherAlpha", "PielouEvenness",
            "LUI_2015_2018", "SoilTypeFusion","slope" ,"aspect", 'LAI',
            'B2','B3', 'B4', 'B5', 'B6', 'B7','B8','B8A', 'B11', 'B12',
            'VHMax_May', 'VVMax_May','NDVI.y', 'VVStd', 'VHStd'    
            )

BexisFV<-Bexis_S2Metrics[MyVars]
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
            "LUI_2015_2018", "SoilTypeFusion","slope" ,"aspect", 'LAI',
            'EVI','SAVI', 'GNDVI', 'ARVI', 'CHLRE', 'MCARI','NDII','MIRNIR', 'MNDVI',
            'VHMax_May', 'VVMax_May','NDVI.y', 'VVStd', 'VHStd'  
            )

BexisIndices<-BexisFVnum[MyVars2]
str(BexisIndices)

write.csv(BexisIndices, file = 'BexisMaxNDVIComposite.csv')

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
             "slope" ,"aspect",'EVI','SAVI',
             'GNDVI', 'ARVI', 'CHLRE', 'MCARI','NDII','MIRNIR','MNDVI',
             'VHMax_May', 'VVMax_May','NDVI.x', 'VVStd', 'VHStd'  
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
setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/RadarPlot')
write.csv(vars_mean, file = paste(explo1,'total', 
                                  #Year1, 
                                  '.csv'))


#############################################################################################################
####################################  Apply Random Forest ###################################################
#############################################################################################################

#We still have NAs in some SAR data, but we will use the na.action=na.omit from RandomForest

###########################################  Biomass ########################################################

#Take the response variable and the predictors
#Choose all, or per site Bexis7Hai, Bexis7Alb, Bexis7Sch

ForRF <- BexisIndices3[c("biomass_g"
                #  ,'x'
                #  ,'y'
                  ,'explo'
                  ,'Year'
                  ,'slope'
                #  ,'sspect'
                  ,'SoilTypeFusion'
                #  ,'LUIgroup'
                  ,'NDVI.y', 'EVI','SAVI', 'GNDVI', 'ARVI', 'CHLRE', 'MCARI','NDII','MIRNIR', 'MNDVI'
                  ,'VVStd', 'VHStd'
                  ,'VHMax_May','VVMax_May'
                #  ,'Phase'
                #  ,'Amp'
)]

# Assign the data to training and validation 3 years training, 1 year validation
training <- subset(ForRF, Year != '2020')
training <- training[ , !(names(training) %in% 'Year')]

validation <- subset(ForRF, Year == '2020')
validation <- validation[ , !(names(validation) %in% 'Year')]

          ############        OR        #################

# Assign the data to training and validation 2 sites training, 1 sites validation
training <- subset(ForRF, explo != 'HAI')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'HAI')
validation <- validation[ , !(names(validation) %in% 'explo')]

           ############        OR        #################

set.seed(21)

# Generate a random sample of "data_set_size" indexes
data_set_size <- floor(nrow(ForRF)/4)
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation <- ForRF[indexes,]

          ############                   #################

dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = biomass_g ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for biomass prediction" )

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(x= pred, y = validation$biomass_g, main = "Biomass Predicted vs Validated for S2")
RMSE <- sqrt(sum((pred - validation$biomass_g)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$biomass)) 

#identify(x= pred, y = validation1$biomass)
# weirdvalues<-Bexis7[c(49,  72, 114),]
# print(weirdvalues)

#ALB for validation RMSE=63, adjusted R2 = 34
#HAI for validation RMSE= 96 , adjusted R2 = 29
#SCH for validation RMSE = 75, adjusted R2 = 49

#2020 for validation RMSE = 74, adjusted R2 = 46
#without explo, RMSE = 74, adjusted R2 = 35, and soil type and slope get more important
#with S1 Max and std RMSE = 68, adjusted R2 = 35. With explo, adjusted r is 45, but RMSE remains
########################################### Spp Richness #####################################################

#Take the response variable and the predictors
ForRF <- Bexis7[c("Rich"
                  ,'year'
                  ,'explo'
                  ,'Slope'
                  ,'Aspect'
                  ,'Soil'
                  ,'LUIgroup'
                  ,'LAI', 'NDVI', 'NDII'
                  ,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian'
                  ,'VVStd', 'VHStd'
                  ,'VHMedian_May','VVMedian_May'
                  ,'Phase', 'Amp'
)]

#Divide data in training (first 3 years) and calidation (last year)
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]

validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

          ##############    OR    ##############

# Assign the data to training and validation 2 sites training, 1 sites validation
training <- subset(ForRF, explo != 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'SCH')
validation <- validation[ , !(names(validation) %in% 'explo')]

dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = Rich ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for spp richness prediction")

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(pred, validation$Rich, main = "Spp richness Predicted vs Validated S1 & S2")
RMSE <- sqrt(sum((pred - validation$Rich)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$Rich)) 

###########################################  Shannon  #####################################################
#Take the response variable and the predictors
ForRF <- Bexis7[c("Shann"
                  ,'year'
                  ,'explo'
                  ,'Slope' 
                  #,'Aspect'
                  ,'LAI', 'NDVI'
                  #,'VHMax','VHMin','VVMax','VVMin','VVMedian','VHMedian'
                  ,'VVStd','VHStd' 
                  #,'VHMedian_May','VVMedian_May'
                  ,'Phase','Amp'
)]

#Divide data in training (first 3 years) and calidation (last year)
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]


validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

###############    OR       ###############

training <- subset(ForRF, explo == 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'HAI')
validation <- validation[ , !(names(validation) %in% 'explo')]
dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = Shann ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for Shannon index prediction")

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(pred, validation$Shann, main = "Shannon Predicted vs Validated S1 & S2")

RMSE <- sqrt(sum((pred - validation$Shann)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$Shann)) 

###########################################  Simpson  #####################################################

#Take the response variable and the predictors
ForRF <- Bexis7[c("Simps"
                  ,'year'
                  ,'Slope' 
                  #,'Aspect'
                  ,'LAI', 'NDVI', 'NDII'
                  #,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian'
                  ,'VVStd', 'VHStd' 
                  #,'VHMedian_May','VVMedian_May'
                  ,'Phase', 'Amp'
)]

#Divide data in training (first 3 years) and calidation (last year)
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]

validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

###############    OR       ###############

training <- subset(ForRF, explo == 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'HAI')
validation <- validation[ , !(names(validation) %in% 'explo')]
dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = Simps ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for Simpsons index prediction")

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(pred, validation$Simps, main = "Simpsons Predicted vs Validated S1 & S2")

RMSE <- sqrt(sum((pred - validation$Simps)^2)/length(pred))
RMSE

#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$Shann)) 

###########################################  Fisher Alpha  #####################################################

#Take the response variable and the predictors
ForRF <- Bexis7[c("alpha"
                  ,'year'
                  ,'explo'
                  ,'Slope' 
                  #,'Aspect'
                  ,'LAI', 'NDVI', 'NDII'
                  #,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian'
                  ,'VVStd', 'VHStd' 
                  #,'VHMedian_May','VVMedian_May'
                  , 'Phase', 'Amp'
)]

#Divide data in training (first 3 years) and calidation (last year)
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]

validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = alpha ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for alpha diversity")

rf
pred <- predict(rf, newdata=validation)
plot(pred, validation$alpha, main = "Fisher's alpha Predicted vs Validated S1 & S2")

RMSE <- sqrt(sum((pred - validation$alpha)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$alpha)) 

###########################################  Evenness  #####################################################

#Take the response variable and the predictors
ForRF <- Bexis7[c("Evenn"
                  ,'year'
                  ,'explo'
                  ,'Slope' 
                  ,'Aspect'
                  ,"LAI", 'NDVI'
                  ,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian'
                  ,'VVStd', 'VHStd' 
                  ,'VHMedian_May','VVMedian_May'
                  ,'Phase', 'Amp'
)]

#Divide data in training (first 3 years) and calidation (last year)
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]

validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

###############    OR       ###############

training <- subset(ForRF, explo == 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'HAI')
validation <- validation[ , !(names(validation) %in% 'explo')]

dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = Evenn ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for Pielu Evenness")

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(pred, validation$Evenn, main = "Shannon Predicted vs Validated S1 & S2")

RMSE <- sqrt(sum((pred - validation$Evenn)^2)/length(pred))
RMSE

#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$Evenn)) 

###########################################  Vascular  #####################################################

#Take the response variable and the predictors
ForRF <- Bexis7[c("vascular"
                  #,'year',
                  ,'explo'
                  ,'Slope', 'Aspect', 'Soil'
                  ,'LAI', 'NDVI', 'NDII'
                  #,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian',
                  #'VVStd', 'VHStd' ,
                  #'VHMedian_May','VVMedian_May'
                  ,'Phase','Amp'
)]

ForRF <- subset(ForRF, vascular !='NA')

#Divide data in training (first 3 years) and calidation (last year)
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]


validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

    ###############    OR       ###############

training <- subset(ForRF, explo == 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'HAI')
validation <- validation[ , !(names(validation) %in% 'explo')]

dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = vascular ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for n vascular plants")

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(pred, validation$vascular, main = "N of vascular plants vs Validated S1 & S2")

RMSE <- sqrt(sum((pred - validation$vascular)^2)/length(pred))
RMSE

#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation$vascular))

