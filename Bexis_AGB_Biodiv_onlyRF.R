################################################################

# Preprocess the bexis data and run and Random Forest Regression
# to predict biomass and biodiversity data

################################################################

library(readxl)
library(readr) 
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

setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model')
dir()
Bexis_clean <- read.csv("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_Biodiv_bexis_forRF.csv")
#View(SE)
str(Bexis_clean)

#############################################################################################################
####################################  Apply Random Forest ###################################################
#############################################################################################################

#We still have NAs in some SAR data, but we will use the na.action=na.omit from RandomForest

###########################################  Biomass ########################################################

#Take the response variable and the predictors
#Choose all, or per site Bexis7Hai, Bexis7Alb, Bexis7Sch

ForRF <- Bexis_clean[c("biomass"
                #  ,'x'
                #  ,'y'
                  ,'explo'
                #  ,'year'
                #  ,'Slope'
                #  ,'Aspect'
                  ,'Soil'
                #  ,'LUIgroup'
                  ,'LAI', 'NDVI', 'NDII'
                #  ,'VVStd'
                #  ,'VHStd'
                # ,'VHMedian_May','VVMedian_May'
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
training <- subset(ForRF, explo != 'SCH')
training <- training[ , !(names(training) %in% 'explo')]

validation <- subset(ForRF, explo == 'SCH')
validation <- validation[ , !(names(validation) %in% 'explo')]

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
print(RMSE/mean(validation$biomass)) 

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

          ##############    OR    ###############

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

