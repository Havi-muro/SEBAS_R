# TODO:   Function in action
# 
# Author: Miguel Alvarez
################################################################################

# To implement a progress bar
library(tcltk)

# Import function script
source("02_as_function.R")

# Read raster files in folder
Files <- list.files(pattern = ".tif")

# Read shapefile with quadrats
uav <- readOGR("uavMegedS.shp")

# Go for a loop with progress bar
Data <- list()

pb <- tkProgressBar(min=0, max=length(Files), width=300)
for(i in Files) {
	Sys.sleep(0.1)
	setTkProgressBar(pb, which(Files == i), title = paste0(which(Files == i),
					" of ", length(Files), " (",
					round(which(Files == i)/length(Files)*100,), "% done)"))
	Data[[i]] <- poly_extract(
			r = raster(i),
			p = uav,
			id = "uav00",
			fun = median)
}
close(pb)

# Since all tables in Data have the same columns, we apply rbind
Data <- do.call(rbind, Data)
