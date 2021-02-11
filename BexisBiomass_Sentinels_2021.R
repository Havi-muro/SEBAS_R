################################################################

# Preprocess the bexis data and run and Random Forest Regression
# to predict biomass and biodiveristy data

################################################################
####
###
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

setwd('C:/Users/Janny/Desktop/SEBAS/Bexis')
dir()
bio_nutr <- read_excel("AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4.xlsx", sheet = 'Sheet1')
#View(SE)
str(bio_nutr)

#############################################################################################################
######################################    DATA EXPLORATION    ###############################################
#############################################################################################################

#rename and remove cloudy plots
bio_nutr2 <- subset(bio_nutr, SW == 2)
#Remove cases where BiomassDOY<CutDOY<S2 =IF(AND(J2<AH2,AH2<AG2),TRUE,FALSE)
bio_nutr3 <- subset(bio_nutr2 , BCS2 == FALSE)
#Remove cases where S2<CutDOY<BiomassDOY =IF(AND(AG2<AH2, AH2<J2),TRUE,FALSE)
bio_nutr4 <- subset(bio_nutr3 , S2CB == FALSE)

#we take out Sch because of too much mulch
#bio_nutr4 <- subset(bio_nutr4, expl !='S') 

#Clump land use indices into 4 groups
LUIgroup <- cut(bio_nutr4$LUI_2015_2018,
                                       breaks =c (0,1,2,3,4),
                                       labels = c('very low', 'low', 'medium', 'high'))
bio_nutr5 <- cbind(bio_nutr4, LUIgroup)

#Change names and write year and editor as factor
BexisS2 <- data.frame(
 PlotID        = bio_nutr5$Useful_EP_PlotID,
 explo         = factor(bio_nutr5$explo),
 year          = factor(bio_nutr5$Year),
# DOY_releves   = bio_nutr5$DOY_releves,
# DOY_biomass   = bio_nutr5$DOY_bm,
# editor        = factor(bio_nutr5$editor),
 height        = as.numeric(bio_nutr5$vegetation_height_mean_cm),
 Pschrubs      = bio_nutr5$cover_shrubs_pc,
 Pherbs        = bio_nutr5$cover_herbs_pc,
 Pbryoph       = bio_nutr5$cover_bryophytes_pc,
 Plich         = bio_nutr5$cover_lichens_pc,
 Plitter       = bio_nutr5$cover_litter_pc,
 Pdeadwood     = bio_nutr5$cover_deadwood_pc,
 Procks        = bio_nutr5$cover_rocks_pc,
 Pbare         = bio_nutr5$cover_bare_soil_pc,
 vascular      = as.numeric(bio_nutr5$number_vascular_plants),
 vasc_cum      = as.numeric(bio_nutr5$cover_cumulative_all_vascular_plants),
 biomass       = bio_nutr5$biomass_g,
LUIgroup       =bio_nutr5$LUIgroup,
Soil           =factor(bio_nutr5$SoilTypeFusion),
Slope          =bio_nutr5$slope,
Aspect         = bio_nutr5$aspect,
  Rich          = as.numeric(bio_nutr5$SpecRichness),
  Shann         = as.numeric(bio_nutr5$Shannon),
  Simps         = as.numeric(bio_nutr5$Simpson),
  invSimps      = as.numeric(bio_nutr5$invSimpson),
  uniSimps      = as.numeric(bio_nutr5$uniSimpson),
  alpha          = as.numeric(bio_nutr5$FisherAlpha),
  Evenn         = as.numeric(bio_nutr5$PielouEvenness),
  K             = bio_nutr5$K,
  ADL           = bio_nutr5$ADL,
  ADF           = bio_nutr5$ADF,
  ADF_1         = bio_nutr5$ADF_2,
  C             = bio_nutr5$C,
  N             = bio_nutr5$N,
  P             = bio_nutr5$P,
  CA            = bio_nutr5$CA,
  MG            = bio_nutr5$MG,
  SW            = factor(bio_nutr5$SW),
  LAI           = bio_nutr5$LAI,
  cab           = bio_nutr5$cab,
  cw            = bio_nutr5$cw,
  fpar          = bio_nutr5$fpar,
  fcover        = bio_nutr5$fcover,
  S2b2          = bio_nutr5$S2b2,
  S2b3          = bio_nutr5$S2b3,
  S2b4          = bio_nutr5$S2b4,
  S2b5          = bio_nutr5$S2b5,
  S2b6          = bio_nutr5$S2b6,
  S2b7          = bio_nutr5$S2b7,
  S2b8          = bio_nutr5$S2b8,
  S2b8a         = bio_nutr5$S2b8a,
  S2b11         = bio_nutr5$S2b11,
  S2b12         = bio_nutr5$S2b12,
  NDVI          = bio_nutr5$NDVI,
  NDII         = bio_nutr5$NDII,
  VHMax        = bio_nutr5$VHMax,
  VHMedian     = bio_nutr5$VHMean,
  VHMin        = bio_nutr5$VHMin,
  VHStd        = bio_nutr5$VHStd,
  VVMax        = bio_nutr5$VVMax,
  VVMedian     = bio_nutr5$VVMean,
  VVMin        = bio_nutr5$VVMin,
  VVStd        = bio_nutr5$VVStd,
VHMax_May        = bio_nutr5$VHMax_May,
VHMedian_May     = bio_nutr5$VHMean_May,
VHMin_May        = bio_nutr5$VHMin_May,
VHStd_May        = bio_nutr5$VHStd_May,
VVMax_May        = bio_nutr5$VVMax_May,
VVMedian_May     = bio_nutr5$VVMean_May,
VVMin_May        = bio_nutr5$VVMin_May,
VVStd_May        = bio_nutr5$VVStd_May,
Phase          = bio_nutr5$Phase,
Amp            = bio_nutr5$Amp
  )
str(BexisS2)

##################   BIOMASS ########################

#Are there NAs in biomass?
is.na(BexisS2$biomass)   
sum(is.na(BexisS2$biomass))
colSums(is.na(BexisS2))

#Are there NAs in LAI?
is.na(BexisS2$LAI)   
sum(is.na(BexisS2$LAI))
colSums(is.na(BexisS2))

# Remove missing values:
BexisS2_1 <- subset(BexisS2, biomass != "NA")
BexisS2_2<- subset(BexisS2_1, LAI != "NA")
BexisS2_2 <- subset(BexisS2_2, VHMax != 'NA')

#Plot to discover outliers and remove them
plot(BexisS2_2$S2b3)
identify(x=BexisS2_2$S2b3)

BexisS2_3 <- BexisS2_2[-c(279, 324), ]
plot(BexisS2_3$S2b2)

plot(BexisS2_3$LAI)
identify(x=BexisS2_3$LAI)
#Some outliers in LAI remain, but we'll let them be
str(BexisS2_3)

#Export for DL comparison
#write.csv(BexisS2_3, "AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4_4DLfromRF.csv")

#Post-hoc testing, to see if there are observer, year or location effect.
p <- ggplot(data = BexisS2_3,
             aes(	x = LAI, 
                  y = biomass,
                  col = explo, 
                  group = explo,))
p <- p + geom_point()
p <- p + xlab("LAI") + ylab("biomass")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(data = BexisS2_3, 
            method = "lm", 
            se = FALSE, 
            aes(y = biomass, 
                x = LAI, 
                group = explo,  
                col = explo))  
p <- p + facet_grid(year ~ explo, scales = "fixed")
p

G <- ggplot(data = BexisS2_3,
            aes( x = LAI, 
                 y = biomass,
                 col = LUIgroup, 
                 group = LUIgroup,))
G <- G + geom_point()
G <- G + xlab("LAI") + ylab("biomass")
G <- G + theme(text = element_text(size=15))
G <- G + geom_smooth(data = BexisS2_3, 
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

BexisS2_3Hai<- subset(BexisS2_3, explo == "HAI")
BexisS2_3Alb<- subset(BexisS2_3, explo == "ALB")
BexisS2_3Sch<- subset(BexisS2_3, explo == "SCH")
BexisS2_3AlbHai<- subset(BexisS2_3, explo != "SCH")
#############################################################################################################
####################################  Apply linear model ####################################################
#############################################################################################################

Model1 <- lm(biomass ~ VVStd + VVMin + VVMax + VVMedian  + VHStd + VHMin + VHMedian + VHMax + LAI 
              ,  data = BexisS2_3)
summary(Model1)
#  biomass_i ~ N(mu_i, 92.33^2) standard error
#  E(biomass_i) = mu_i  
#  mu_i = -42.71250 + -7.38617 * LAI ==> fitted line = intercept + slope * X
#  adjusted R2 = 0.4367

#Apply model selection
step(Model1)
# Refit the model with result of stepwise: Call:
#Model2 <- lm(formula = biomass ~ S2b2 + S2b4 + S2b5 + S2b6 + S2b8 + S2b8a + 
# S2b11 + S2b12, data = BexisS2_3)
#summary(Model2)




#############################################################################################################
####################################  Apply Random Forest ###################################################
#############################################################################################################

#We still have NAs in some SAR data, but we will use the na.action=na.omit from RandomForest

###########################################  Biomass ########################################################

#Take the response variable and the predictors
#Choose all, or per site BexisS2_3Hai, BexisS2_3Alb, BexisS2_3Sch

ForRF <- BexisS2_3[c("biomass"
                     ,'Slope', 
                     #'Aspect', 
                     'Soil', #'LUIgroup',
                    "LAI", 'NDVI', 'NDII',
                    'VVStd', 'VHStd' 
                   #, 'VHMedian_May','VVMedian_May'
                    #, 'Phase' 
                   #'Amp'
)]

# Set random seed to make results reproducible:
set.seed(48)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

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
pred <- predict(rf, newdata=validation1)
plot(x= pred, y = validation1$biomass, main = "Biomass Predicted vs Validated for S1 & S2")
RMSE <- sqrt(sum((pred - validation1$biomass)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation1$biomass)) 

#identify(x= pred, y = validation1$biomass)
# weirdvalues<-BexisS2_3[c(49,  72, 114),]
# print(weirdvalues)

########################################### Spp Richness #####################################################

is.na(BexisS2_3$Rich)   
sum(is.na(BexisS2_3$Rich))
colSums(is.na(BexisS2_3))

# Remove missing values:
BexisS2_4 <- subset(BexisS2_3, Rich != "NA")
BexisS2_4 <- subset(BexisS2_4, explo == "SCH")


#Take the response variable and the predictors
ForRF <- BexisS2_4[c("Rich"
                     ,'Slope',
                   #  'Aspect',
                     'Soil', 'LUIgroup'
                     ,"LAI", 'NDVI', 'NDII',
                    # 'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian',
                    'VVStd', 'VHStd' ,
                    # 'VHMedian_May','VVMedian_May'
                      'Phase', 'Amp'
)]

# Set random seed to make results reproducible:
set.seed(18)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

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
pred <- predict(rf, newdata=validation1)
plot(pred, validation1$Rich, main = "Spp richness Predicted vs Validated S1 & S2")
RMSE <- sqrt(sum((pred - validation1$Rich)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation1$Rich)) 

###########################################  Shannon  #####################################################

is.na(BexisS2_3$Shann)   
sum(is.na(BexisS2_3$Shann))
colSums(is.na(BexisS2_3))

# Remove missing values:
BexisS2_4 <- subset(BexisS2_3, Shann != "NA")
BexisS2_4 <- subset(BexisS2_4, explo == "ALB")

#Take the response variable and the predictors
ForRF <- BexisS2_4[c("Shann"
                     ,'Slope' 
                     #,'Aspect'
                     ,"LAI", 'NDVI'
                     #,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian'
                     ,'VVStd', 'VHStd' 
                     #, 'VHMedian_May','VVMedian_May'
                     , 'Phase', 'Amp'
)]


# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

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
pred <- predict(rf, newdata=validation1)
plot(pred, validation1$Shann, main = "Shannon Predicted vs Validated S1 & S2")

RMSE <- sqrt(sum((pred - validation1$Shann)^2)/length(pred))
RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
print(RMSE/mean(validation1$Shann)) 

###########################################  Simpson  #####################################################

#Take the response variable and the predictors
ForRF <- BexisS2_3[c("Simps",
                     "S2b2", "S2b3", "S2b4", "S2b5", "S2b6", "S2b7", "S2b8", "S2b8a", "S2b11", "S2b12",
                     "LAI", 'cab', 'fcover', 'fpar', 'cw',
                     'VVStd', 'VVMin', 'VVMax', 'VVMedian', 'VHStd', 'VHMin', 'VHMedian', 'VHMax'
)]


# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

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
pred <- predict(rf, newdata=validation1)
plot(pred, validation1$Shann, main = "Simpsons index Predicted vs Validated S1 & S2")



###########################################  Fisher Alpha  #####################################################

#Take the response variable and the predictors
ForRF <- BexisS2_3[c("alpa"
                     ,'Slope', 'Aspect'
                     ,"LAI", 'NDVI'
                     ,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian','VVStd', 'VHStd' 
                     , 'VHMedian_May','VVMedian_May'
                     , 'Phase', 'Amp'
)]


# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = alpa ~ .,
  data=training, 
  ntree=500,
  importance=TRUE,
  na.action = na.omit
)
varImpPlot(rf, main = "Accuracy and Gini index for alpha diversity")



#Use the validation dataset to validate the model.
rf
pred <- predict(rf, newdata=validation1)
plot(pred, validation1$alpa, main = "Alpha diversity Predicted vs Validated S1 & S2")


###########################################  Evenness  #####################################################

#Take the response variable and the predictors
ForRF <- BexisS2_3[c("Evenn",
                     "S2b2", "S2b3", "S2b4", "S2b5", "S2b6", "S2b7", "S2b8", "S2b8a", "S2b11", "S2b12",
                     "LAI", 'cab', 'fcover', 'fpar', 'cw',
                     'VVStd', 'VVMin', 'VVMax', 'VVMedian', 'VHStd', 'VHMin', 'VHMedian', 'VHMax'
)]


# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

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
pred <- predict(rf, newdata=validation1)
plot(pred, validation1$Evenn, main = "Pielu Evenness Predicted vs Validated S1 & S2")

###########################################  Vascular  #####################################################

#Take the response variable and the predictors
ForRF <- BexisS2_3[c("vascular"
                     ,'Slope', 'Aspect', 'Soil'
                     ,"LAI", 'NDVI'
                     ,'VHMax','VHMin', 'VVMax','VVMin','VVMedian','VHMedian','VVStd', 'VHStd' 
                     , 'VHMedian_May','VVMedian_May'
                     , 'Phase', 'Amp'
)]


# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation1 <- ForRF[indexes,]
dim(training)
dim(validation1)

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
pred <- predict(rf, newdata=validation1)
plot(pred, validation1$vascular, main = "N vascular plants Predicted vs Validated S1 & S2")



####################################   Model Validation  ################################
# 1. Homogeneity
Res1 <- resid(Model2)  #Get residuals
Fitt1 <- fitted(Model2) #Get fitted values

# Plot residuals vs fitted values
plot(x = Fitt1, 
     y = Res1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)
# Cone shape pattern, there is heterogeneity. What do we do?


# Influential observations
par(mfrow = c(1, 1))
plot(cooks.distance(Model2), type = "h", ylim = c(0, 1))
abline(h = 1)
# No influential observations, no samples reach threshold level 1

# 4. Normality
par(cex.lab = 1.5, mar = c(5,5,2,2))
Res1 <- resid(Model2)
hist(Res1, breaks = 15, xlab = "Residuals", main = "")
# It doesn't get better than this in ecology!

# 2. Independence
#  -Plot residuals vs each covariate in the model
#  -Plot residuals vs each covariate not in the model

#Hasta aqui llegu?.
BexisS2_3$Res1 <- Res1   #Put Residuals inside the databse
MySel <- c("S2b2" , "S2b4", "S2b5" , "S2b6" ,"S2b8",  "S2b8a", "S2b11", "S2b12")
MyMultipanel.ggp2(Z = Model2, 
                  varx = S2b2, 
                  vary = "Res1", 
                  ylab = "Residuals",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = TRUE) 
# There seems to be a non-linear pattern in the residuals. 
# We are not focussing on the edges here...but at weight = 18-ish. 
# That is probably the reason why people always log transform 
# these variables.

# Plot residuals vs explo
par(cex.lab = 1.5, mar = c(5,5,2,2))
boxplot(Res1 ~ explo, 
        data = BexisS2_3,
        ylab = "Residuals")
abline(h = 0, lty = 2 )

# Plot residuals vs year
par(cex.lab = 1.5, mar = c(5,5,2,2))
boxplot(Res1 ~ year, 
        data = BexisS2_3,
        ylab = "Residuals")
abline(h = 0, lty = 2 )

# Plot residuals versus S2b2...for each year

p <- ggplot()
p <- p + geom_point(data = Sparrows3, 
                    aes(y = E1, x = Wt),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Weight") + ylab("Residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = Sparrows3, 
                     #method = "lm",
                     aes(x = Wt, y = E1))
p <- p + facet_grid(. ~ Sex, scales = "fixed")
p  

# Another model validation tool: Plot fitted values vs observed values
par(cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = F1,
     y = Sparrows3$Wingcrd,
     xlab = "Fitted values",
     ylab = "Observed Wingcrd data",
     xlim = range(Sparrows3$Wingcrd),
     ylim = range(Sparrows3$Wingcrd))
abline(a = 0, b = 1)
# If this is on the 45 degree line, then we have a good fit.



#Generate 100 random values for LAI per exploratory and per year 
#using min & max of LAI
MyRandom <- ddply(BexisS2_3, 
                  .(explo, year), 
                  summarize,
                  S2b2 = seq(from = min(S2b2), 
                             to = max(S2b2), 
                             length = 100))
head(MyRandom, 10)

# Predict the biomass for random LAIs.
P1 <- predict(Model2, newdata = MyRandom, se = TRUE)



#to plot residuals
#Plot by exploratory and by year or editor
p <- ggplot()
p <- p + geom_point(data = BexisS2_3, 
                    aes(y = biomass, x = LAI),
                    shape = 1, 
                    size = 1)
p <- p + xlab("LAI") + ylab("biomass")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_line(data = MyData, 
                   aes(x = LAI, y = Pred), 
                   colour = "black")
p <- p + geom_ribbon(data = MyData, 
                     aes(x = LAI, 
                         ymax = seup, 
                         ymin = selo),
                     alpha = 0.5)
p <- p + facet_grid(Sex ~ fObserver, scales = "fixed")
p 