library(rgdal)
library(raster)
library(sp)
library(rgeos)

#get dataset
setwd('C:/Users/Janny/Desktop/SEBAS/GIS')
path<-'C:/Users/Janny/Desktop/SEBAS/GIS'
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


