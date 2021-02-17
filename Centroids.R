library(rgdal)
library(raster)
library(sp)
library(rgeos)

#get dataset
setwd('C:/Users/Janny/Desktop/SEBAS/GIS')
path<-'C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model'
uav <- readOGR("C:/Users/Janny/Desktop/SEBAS/GIS/UAVPlots.shp")
head(uav)

#convert polygons to centroid and coherce int a df
centr <- (gCentroid(uav, byid=TRUE))
centr2<-SpatialPointsDataFrame(centr, data.frame(ID=1:length(centr)))

plot(centr)
head(centr)

#get coordinates and bind to initial shapefile
centr3<-geom(centr2)
centr4 <- cbind(uav, centr3)
writeOGR(centr4, path, "uavcentrcoord", driver='ESRI Shapefile')

#Write as csv to paste manually because merge with bexis raw dataset doesnt work
centr5<-as.data.frame(centr4)
write.csv2(centr5, 'C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/uavcentrcoord.csv')


BexisRaw <- as.data.frame(read_excel("C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_4.xlsx", sheet = 'Sheet1'))

BexisRawCoord<-merge(centr4, BexisRaw, by='ep', duplicateGeoms=TRUE)

write.csv2(BexisRawCoord, 'C:/Users/Janny/Desktop/SEBAS/Bexis/Bexis_BiomDiversity_Model/AGB_PFT_Biodiv_NPK_S2S1_2017-2020_SoilTypes_coord.csv')
