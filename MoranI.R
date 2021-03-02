################################################################

# Preprocess the bexis data and run Moran's I

################################################################
library(ape)
library(geoR)
library(readxl)
library(sp)
library(nlme)

setwd('C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model')
dir()
BexisRaw <- read_excel("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4.xlsx", sheet = 'Sheet1')
#View(SE)
str(BexisRaw)

#############################################################################################################
######################################    DATA EXPLORATION    ###############################################
#############################################################################################################

#rename and remove cloudy plots
Bexis2 <- subset(BexisRaw, SW == 2)
#Remove cases where BiomassDOY<CutDOY<S2 =IF(AND(J2<AH2,AH2<AG2),TRUE,FALSE)
Bexis3 <- subset(Bexis2 , BCS2 == FALSE)
#Remove cases where S2<CutDOY<BiomassDOY =IF(AND(AG2<AH2, AH2<J2),TRUE,FALSE)
Bexis5 <- subset(Bexis3 , S2CB == FALSE)

#Change names and write year and editor as factor
Bexis6 <- data.frame(
  x             = Bexis5$x,
  y             = Bexis5$y,
  year          = Bexis5$Year,
  explo         = Bexis5$explo,
  height        = as.numeric(Bexis5$vegetation_height_mean_cm),
  vascular      = as.numeric(Bexis5$number_vascular_plants),
  biomass       = Bexis5$biomass_g,
  Rich          = as.numeric(Bexis5$SpecRichness),
  alpha         = as.numeric(Bexis5$FisherAlpha),
  Evenn         = as.numeric(Bexis5$PielouEvenness),
  LAI           =as.numeric(Bexis5$LAI)
  )
str(Bexis6)

##################   BIOMASS ########################

#Are there NAs in biomass?
is.na(Bexis6$biomass)   
sum(is.na(Bexis6$biomass))
colSums(is.na(Bexis6))

# Remove missing values:
Bexis7 <- subset(Bexis6, biomass != "NA")
#Bexis7 <- subset(Bexis6, Rich != "NA")
Bexis7 <- subset(Bexis7, LAI != 'NA')

#Select single year
Bexis8 <- subset(Bexis7, year == '2019')
Bexis8 <- subset(Bexis8, explo == 'ALB')

#Select columns to build distance matrix
BexisAGB <-Bexis8[c(7,11,1,2)]
str(BexisAGB)
AGB.dist <-as.matrix(dist(cbind(BexisAGB$x, BexisAGB$y)))
AGB.dist.inv <- 1/AGB.dist
diag(AGB.dist.inv) <- 0
     
AGB.dist.inv[1:5, 1:5]

# Apply Moran's I
# If the observed value of I is significantly greater than the expected value, 
# then the values of x are positively autocorrelated, 
# whereas if Iobserved <<< Iexpected, this will indicate negative autocorrelation.

# H null is that there is no spatial autocorrelation and that the distribution is random
MoranI<-Moran.I(BexisAGB$biomass, AGB.dist.inv)
MoranI

ZI <- (MoranI$observed - MoranI$expected)/sqrt(MoranI$sd)
ZI

# -1.65 < ZI < 1.65, therfore there is a high chance that the  pattern is random
# we accept H null

# Generate variogram
# measure of the degree of similarity between pairs of points separated by a specific distance
BexisAGB.gls <- gls(biomass ~ LAI, BexisAGB, method='REML')
plot(BexisAGB.gls) #no obvious signs of issues with normality or homogeneity of variance. 

plot(Variogram(BexisAGB.gls, form = ~y+x, resType='normalized')) #semivariance increases with distance
