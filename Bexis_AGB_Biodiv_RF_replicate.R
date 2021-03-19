################################################################

# Run RF regression multiple times with random distribution 
# of train and validation
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
library(caret)
library(data.table)
library(matrixStats)
library(parallel, lib.loc = "C:/Program Files/R/R-4.0.2/library")
library(tictoc)
require(caTools)
library(caTools)

setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model')
dir()
Bexis_clean <- read.csv("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/BexisMaxNDVIComposite.csv")
#View(SE)
str(Bexis_clean)

#############################################################################################################
####################################  Apply Random Forest ###################################################
#############################################################################################################

###########################################  Biomass ########################################################

#Take the response variable and the predictors
#Choose all, or per site Bexis7Hai, Bexis7Alb, Bexis7Sch

RFvars <- c(#"Year",
                       #"ep",
                       #"explo",
                       #"x",
                       #"y",
                       "PielouEvenness",
                      #"LUI_2015_2018",
                      "SoilTypeFusion",
                        "slope",
                      "aspect",
                      "LAI",
                       "EVI",
                      "SAVI",
                       "GNDVI",
                      "ARVI",
                       "CHLRE",
                      "MCARI",
                      "NDII",
                       "MIRNIR",
                       "MNDVI",
                       "VHMax_May",
                      "VVMax_May",
                       "NDVI.y"
                      #"VVStd","VHStd"
)

ForRF <- Bexis_clean[RFvars]
ForRF <-na.omit(ForRF)

do_once <- function()
{
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(ForRF)/3)

#Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(ForRF), size = data_set_size)

# Assign the data to training and validation
training <- ForRF[-indexes,]
validation <- ForRF[indexes,]

# Find the best number of variables tried at each split 
# mtry <- tuneRF(ForRF[-1],ForRF$biomass_g, ntreeTry=500,
#                stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
# best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
#print(mtry)
#print(best.m)

#Run RF for our response variable in our training dataset
rf <- randomForest(
  formula = PielouEvenness ~ .,
  data=training, 
  ntree=500,
#  mtry=best.m,
  importance=TRUE,
  na.action = na.omit
)
#varimplot <-varImpPlot(rf, main = "Accuracy and Gini index for biomass prediction")

#Use the validation dataset to validate the model.
# rf
pred <- predict(rf, newdata=validation)
#plot(x= pred, y = validation$SpecRichness, main = "Biomass Predicted vs Validated for S1 & S2")
RMSE <- sqrt(sum((pred - validation$PielouEvenness)^2)/length(pred))
#RMSE
#divide it by the mean of our outcome variable so we can interpret RMSE in terms of percentage of the mean:
RRMSE <- RMSE/mean(validation$PielouEvenness)
RRMSE

}
#################################################################################
# Variable importance
# Replicate function several times

# Function to do replicate several times (Use RepParallel() instead of replicate())
RepParallel <- function(n, expr, simplify = "array",...) {
  answer <-
    mclapply(integer(n), eval.parent(substitute(function(...) expr)),...)
  if (!identical(simplify, FALSE) && length(answer)) 
    return(simplify2array(answer, higher = (simplify == "array")))
  else return(answer)
}

# The result stored is a list with the last output paramter of the function
tic()
varImportance <- RepParallel(1000, do_once())
toc()


#varImportance
varimp_df<-as.data.frame(varImportance)

# We will print many plots. Select only the IncMSE variables
VarImp_df2 <- varimp_df[,grep('%IncMSE', names(varimp_df))]

avgVarImp <- rowMeans(VarImp_df2)
avgVarImp
# For standard deviations we need to pass it as matrix
matrix <- as.matrix(VarImp_df2)
sdVarImp <- as.data.frame(rowSds(matrix))

# bind sd and avg
avg_SD_VarImp <- cbind(avgVarImp, sdVarImp)

write.csv(avg_SD_VarImp, file = 'VariableImportancex1000_Evenness.csv')
barplot(avgVarImp)

#################################################################################
# RRMSE
# Replicate function several times.
# The result stored is a list with the last output parameter of the function
tic()
ListRMSE <- RepParallel(1000, do_once())
toc()

median(ListRMSE)
sd(ListRMSE)

#With LAI
#0.61 +-0.05

#Without LAI nor S1, but with slope and soil
#0.62 +-0.044

# Only s2 indices, no LAI
#0.67 +- 0.04

#With explo
# 0.56 +- 0.04












# 
# 
# # Calculate averages IncMSE
# # Comment do_once after varimplot
# varImportance <- replicate(10, do_once())
# varImportance
# r<-row.names(varimp)
# 
# RFvars <- as.data.table(RFvars[-1])
# 
# VarImp_dt <-as.data.table(varImportance)
# VarImp_dt
# VarImp_dt_changed <-VarImp_dt
# row.names(VarImp_dt_changed) <- c(r)
# 
# VarImp2 <- transpose(cbind(RFvars, VarImp_dt))
# avgVarImp <- rowMeans(VarImp2)



