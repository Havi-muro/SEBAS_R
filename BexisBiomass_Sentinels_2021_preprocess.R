################################################################

# Preprocess the bexis data and run and Random Forest Regression
# to predict biomass and biodiversity data

################################################################

library(readxl)
library(readr) 
library(data.table)
library(GGally)
library(lattice)  
library(ggplot2)
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
library(randomForest)
require(caTools)
library(caTools)

#############################################################################################################
######################################    DATA EXPLORATION    ###############################################
#############################################################################################################

setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model')
dir()

#Open different datasets
BexisRaw <- read_excel("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4.xlsx", sheet = 'Sheet1')
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

#Select variables and transform date to DOY
CutDate_vars <- c('Year', 'EP_PlotID' , 'DateCut1', 'Cuts')
CutDate <- BexisSurvey[CutDate_vars]
str(CutDate)

tmp <- as.POSIXlt(CutDate$DateCut1, format = "%Y-%m-%d")
tmp$yday
CutDOY <- cbind(CutDate, tmp$yday)
str(CutDOY)

#change names of common fields and merge both datsets
names(CutDOY)[names(CutDOY) == "EP_PlotID"] <- "EpPlotID"
names(CutDOY)[names(CutDOY) == "tmp$yday"] <- "CutDOY"

str(CutDOY)

#Merge by several variables
BexisMerge <- merge(BexisRaw, CutDOY, by=c('Year', 'EpPlotID'))
str(BexisMerge)

BexisRecent <- subset(BexisMerge, Year > 2016)

#Plot the cut dates
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

#Calculate stats of the mowing and biomass dates for recent dates
tapply(as.numeric(BexisRecent$DOY_bm), BexisRecent$explo, summary)
tapply(BexisRecent$CutDOY, BexisRecent$explo, summary)

#ALB: mowing is around 21 June
#HAI: mowing is around 12 June
#SCH: mowing is around 28 June
#Biomass collection in Alb is between 12 & 17 May
#Biomass collection in Hai is between 11 & 18 May
#Biomass collection in SCH is between 19 & 25 May

#Merge data with satellite info
Bexis_S2Metrics <- merge(BexisRecent, S2Metrics, by=c('Year','ep'))
str(Bexis_S2Metrics)

#Select the Final Variables we will use
MyVars <- c("Year", "ep", 'explo', "x","y","number_vascular_plants","biomass_g", "SpecRichness", "Shannon", "Simpson", "FisherAlpha", "PielouEvenness",
"LUI_2015_2018", "SoilTypeFusion","slope" ,"aspect", 'NDVI.y', 'B3', 'B4', 'B5', 'B6', 'B7','B8','B8A', 'B11', 'B12')
BexisFV<-Bexis_S2Metrics[MyVars]
str(BexisFV)

BexisFVnum <- transform(BexisFV, number_vascular_plants = as.numeric(number_vascular_plants), 
                        SpecRichness = as.numeric(SpecRichness),
                        Shannon  = as.numeric(Shannon),
                        Simpson = as.numeric(Simpson),
                        FisherAlpha = as.numeric(FisherAlpha),
                        PielouEvenness = as.numeric(PielouEvenness)
                        )

str(BexisFVnum)

####################################################################################
#This is now with the old data from single Sentinel-2 image
#rename and remove cloudy plots
Bexis2 <- subset(BexisRaw, SW == 2)
#Remove cases where BiomassDOY<CutDOY<S2 =IF(AND(J2<AH2,AH2<AG2),TRUE,FALSE)
Bexis3 <- subset(Bexis2 , BCS2 == FALSE)
#Remove cases where S2<CutDOY<BiomassDOY =IF(AND(AG2<AH2, AH2<J2),TRUE,FALSE)
Bexis4 <- subset(Bexis3 , S2CB == FALSE)

#we take out Sch because of too much mulch
#bio_nutr4 <- subset(bio_nutr4, expl !='S') 

#Clump land use indices into 4 groups
LUIgroup <- cut(Bexis4$LUI_2015_2018,
                                       breaks =c (0,1,2,3,4),
                                       labels = c('very low', 'low', 'medium', 'high'))
Bexis5 <- cbind(Bexis4, LUIgroup)

#Change names and write year and editor as factor
Bexis6 <- data.frame(
 ep        = Bexis5$ep,
 explo         = factor(Bexis5$explo),
 year          = factor(Bexis5$Year),
# DOY_releves   = Bexis5$DOY_releves,
# DOY_biomass   = Bexis5$DOY_bm,
# editor        = factor(Bexis5$editor),
 x             = Bexis5$x,
 y             = Bexis5$y, 
 height        = as.numeric(Bexis5$vegetation_height_mean_cm),
 vascular      = as.numeric(Bexis5$number_vascular_plants),
 vasc_cum      = as.numeric(Bexis5$cover_cumulative_all_vascular_plants),
 biomass       = Bexis5$biomass_g,
LUIgroup       =Bexis5$LUIgroup,
Soil           =factor(Bexis5$SoilTypeFusion),
Slope          =Bexis5$slope,
Aspect         = Bexis5$aspect,
  Rich          = as.numeric(Bexis5$SpecRichness),
  Shann         = as.numeric(Bexis5$Shannon),
  Simps         = as.numeric(Bexis5$Simpson),
  invSimps      = as.numeric(Bexis5$invSimpson),
  uniSimps      = as.numeric(Bexis5$uniSimpson),
  alpha         = as.numeric(Bexis5$FisherAlpha),
  Evenn         = as.numeric(Bexis5$PielouEvenness),
  SW            = factor(Bexis5$SW),
  LAI           = Bexis5$LAI,
  cab           = Bexis5$cab,
  cw            = Bexis5$cw,
  fpar          = Bexis5$fpar,
  fcover        = Bexis5$fcover,
  S2b2          = Bexis5$S2b2,
  S2b3          = Bexis5$S2b3,
  S2b4          = Bexis5$S2b4,
  S2b5          = Bexis5$S2b5,
  S2b6          = Bexis5$S2b6,
  S2b7          = Bexis5$S2b7,
  S2b8          = Bexis5$S2b8,
  S2b8a         = Bexis5$S2b8a,
  S2b11         = Bexis5$S2b11,
  S2b12         = Bexis5$S2b12,
  NDVI          = Bexis5$NDVI,
  NDII         = Bexis5$NDII,
  VHMax        = Bexis5$VHMax,
  VHMedian     = Bexis5$VHMean,
  VHMin        = Bexis5$VHMin,
  VHStd        = Bexis5$VHStd,
  VVMax        = Bexis5$VVMax,
  VVMedian     = Bexis5$VVMean,
  VVMin        = Bexis5$VVMin,
  VVStd        = Bexis5$VVStd,
VHMax_May        = Bexis5$VHMax_May,
VHMedian_May     = Bexis5$VHMean_May,
VHMin_May        = Bexis5$VHMin_May,
VHStd_May        = Bexis5$VHStd_May,
VVMax_May        = Bexis5$VVMax_May,
VVMedian_May     = Bexis5$VVMean_May,
VVMin_May        = Bexis5$VVMin_May,
VVStd_May        = Bexis5$VVStd_May,
Phase          = Bexis5$Phase,
Amp            = Bexis5$Amp
  )
str(Bexis6)

##################   BIOMASS ########################

#Are there NAs in biomass?
is.na(Bexis6$biomass)   
sum(is.na(Bexis6$biomass))
colSums(is.na(Bexis6))

#Are there NAs in LAI?
is.na(Bexis6$LAI)   
sum(is.na(Bexis6$LAI))
colSums(is.na(Bexis6))

# Remove missing values:
Bexis7 <- subset(Bexis6, biomass != "NA")
Bexis7<- subset(Bexis7, LAI != "NA")
Bexis7 <- subset(Bexis7, VHMax != 'NA')

#Plot to discover outliers and remove them
plot(Bexis7$S2b3)
identify(x=Bexis7$S2b3)

Bexis7 <- Bexis7[-c(279, 324), ]
plot(Bexis7$S2b2)

plot(Bexis7$LAI)
identify(x=Bexis7$LAI)
#Some outliers in LAI remain, but we'll let them be
str(Bexis7)

#Export for DL comparison
#write.csv(Bexis7, "AGB_Biodiv_bexis_forRF.csv")

#Post-hoc testing, to see if there are observer, year or location effect.
p <- ggplot(data = Bexis7,
             aes(	x = LAI, 
                  y = biomass,
                  col = explo, 
                  group = explo,))
p <- p + geom_point()
p <- p + xlab("LAI") + ylab("biomass")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(data = Bexis7, 
            method = "lm", 
            se = FALSE, 
            aes(y = biomass, 
                x = LAI, 
                group = explo,  
                col = explo))  
p <- p + facet_grid(year ~ explo, scales = "fixed")
p

G <- ggplot(data = Bexis7,
            aes( x = LAI, 
                 y = biomass,
                 col = LUIgroup, 
                 group = LUIgroup,))
G <- G + geom_point()
G <- G + xlab("LAI") + ylab("biomass")
G <- G + theme(text = element_text(size=15))
G <- G + geom_smooth(data = Bexis7, 
                     method = "lm", 
                     se = FALSE, 
                     aes(y = biomass, 
                         x = LAI, 
                         group = LUIgroup,  
                         col = LUIgroup))  
G <- G + facet_grid(LUIgroup ~ explo, scales = "fixed")
G


#Conclusions of data exploration
#Points in SCH seem to have a much larger variance in biomass
#No interaction between sites, which is good.
#Many missing points because of lack of images and other outliers

Bexis7Hai<- subset(Bexis7, explo == "HAI")
Bexis7Alb<- subset(Bexis7, explo == "ALB")
Bexis7Sch<- subset(Bexis7, explo == "SCH")
Bexis7AlbHai<- subset(Bexis7, explo != "SCH")

#############################################################################################################
####################################  Apply Random Forest ###################################################
#############################################################################################################

#We still have NAs in some SAR data, but we will use the na.action=na.omit from RandomForest

###########################################  Biomass ########################################################

#Take the response variable and the predictors
#Choose all, or per site Bexis7Hai, Bexis7Alb, Bexis7Sch

ForRF <- Bexis7[c("biomass"
                #  ,'x'
                #  ,'y'
                  ,'explo'
                #  ,'year'
                  ,'Slope'
                #  ,'Aspect'
                  ,'Soil'
                #  ,'LUIgroup'
                  ,'LAI', 'NDVI', 'NDII'
                  ,'VVStd', 'VHStd'
                #  ,'VHMedian_May','VVMedian_May',
                #  ,'Phase'
                #  ,'Amp'
)]

# Set random seed to make results reproducible:
#set.seed(48)
# Calculate the size of each of the data sets:
#data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
#indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation 3 years training, 1 year validation
training <- subset(ForRF, year != '2020')
training <- training[ , !(names(training) %in% 'year')]

validation <- subset(ForRF, year == '2020')
validation <- validation[ , !(names(validation) %in% 'year')]

          ############        OR        #################

# Assign the data to training and validation 2 sites training, 1 sites validation
training <- subset(ForRF, explo == 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'ALB')
validation <- validation[ , !(names(validation) %in% 'explo')]

           ############        OR        #################

set.seed(21)

# Generate a random sample of "data_set_size" indexes
data_set_size <- floor(nrow(ForRF)/4)
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation <- ForRF[indexes,]


dim(training)
dim(validation)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = biomass ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for biomass prediction" )

#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation)
plot(x= pred, y = validation$biomass, main = "Biomass Predicted vs Validated for S1 & S2")
RMSE <- sqrt(sum((pred - validation$biomass)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation1$biomass)) 

#identify(x= pred, y = validation1$biomass)
# weirdvalues<-Bexis7[c(49,  72, 114),]
# print(weirdvalues)

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

