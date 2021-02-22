# TODO:   Extract statistic from rasters through polygons
# 
# Author: Miguel Alvarez
################################################################################

# library(sp)
library(raster)
library(rgdal)

# List tif files
Files <- list.files(pattern = ".tif")

# Select one file
img <- raster(Files[1])

# Read shapefile with quadrats
uav <- readOGR("uavMegedS.shp")

# Just cross-check on sid
proj4string(uav) == proj4string(img)

# Plot for display
plot(img)
plot(uav, border = "red", add = TRUE)

# Comments:
# - Variable uav00 will be considered as the ID for each plot
# - Input shapefile should be cropped by the raster extension
plot(uav) # before

uav <- crop(uav, extent(img))

plot(uav) # after

# Javier's approach
imgext <- extract(img, uav, fun = median, df = TRUE)

# For a general application, we replace ID's and name of second column
# Specific only for this case
id = "uav00"
colnames(imgext)[colnames(imgext) != "ID"] <- "value"
colnames(imgext)[colnames(imgext) == "ID"] <- id

imgext[ , id] <- uav@data[ , id]

# Actually the task is fulfilled
# BUT we made it a function (see script 02)
# IN script 03 we apply the function
