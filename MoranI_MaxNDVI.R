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
BexisRaw <- read.csv("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/BexisMaxNDVIComposite.csv")
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
  x             = BexisRaw$x,
  y             = BexisRaw$y,
  year          = BexisRaw$Year,
  explo         = BexisRaw$explo,
#  height        = as.numeric(Bexis5$vegetation_height_mean_cm),
#  vascular      = as.numeric(Bexis5$number_vascular_plants),
  biomass       = BexisRaw$biomass_g,
  Rich          = as.numeric(BexisRaw$SpecRichness),
#  alpha         = as.numeric(Bexis5$FisherAlpha),
#  Evenn         = as.numeric(Bexis5$PielouEvenness),
  CHLRE         =as.numeric(BexisRaw$CHLRE)
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
Bexis7 <- subset(Bexis7, CHLRE != 'NA')

#Select single year or single site
#Can't work with multimple samples in same coordinates
Bexis8 <- subset(Bexis7, year == '2017')
Bexis8 <- subset(Bexis8, explo == 'ALB')

#Select columns to build distance matrix
#BexisAGB <-Bexis7[c(biomass, LAI,x,y)]
BexisAGB <-Bexis8[c(5,7,1,2)]
str(BexisAGB)
AGB.dist <-as.matrix(dist(cbind(BexisAGB$x, BexisAGB$y)))
AGB.dist.inv <- 1/AGB.dist

#If we used several samples from same site we might get infinite values in the matrix we have to remove
AGB.dist.inv[is.infinite(AGB.dist.inv)] <- 0

diag(AGB.dist.inv) <- 0
     
AGB.dist.inv[1:5, 1:5]

# Apply Moran's I
# If the observed value of I is significantly greater than the expected value, 
# then the values of x are positively autocorrelated, 
# whereas if Iobserved <<< Iexpected, this will indicate negative autocorrelation.

# H null is that there is no spatial autocorrelation and that the distribution is random
MoranI<-Moran.I(BexisAGB$biomass, AGB.dist.inv, na.rm=TRUE)
MoranI

ZI <- (MoranI$observed - MoranI$expected)/sqrt(MoranI$sd)
ZI

# -1.65 < ZI < 1.65, therfore there is a high chance that the  pattern is random
# we accept H null

# Generate variogram
# measure of the degree of similarity between pairs of points separated by a specific distance
BexisAGB.gls <- gls(biomass ~ CHLRE, BexisAGB, method='REML')
plot(BexisAGB.gls) #no obvious signs of issues with normality or homogeneity of variance. 

plot(Variogram(BexisAGB.gls, form = ~y+x, resType='normalized')) #semivariance increases with distance

