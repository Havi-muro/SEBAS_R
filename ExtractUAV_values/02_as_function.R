# TODO:   A function prepared to be inserted in a package
# 
# Author: Miguel Alvarez
################################################################################

require(raster)
require(rgdal)

#' @name poly_extract
#' 
#' @title Extract raster values using spatial polygons
#' 
#' @description 
#' Extraction of raster statistics for single features included in a
#' [SpatialPolygonDataFrame-class] object.
#' 
#' @param r A [RasterLayer-class] object.
#' @param p A [SpatialPolygonDataFrame-class] including polygons to extract
#'     raster values.
#' @param id A character value indicating the name of the variable in 'p' to be
#'     used as feature ID. If missing, each feature gets a unique integer value.
#' @param fun A function for calculating the statistics. Passed to
#'     [raster::extract()].
#' @param ... Further arguments passed to [raster::extract()].
#' 
#' @return A data frame.
#' 
#' @export 
#' 
poly_extract <- function(r, p, id, fun = mean, ...) {
	# TODO: Use either S4 or S3 method
	# TODO: Extent to RasterStack and RasterBrick classes
	if(proj4string(p) != proj4string(r))
		stop("Projections of 'r' and 'p' have to be identic.")
	p <- crop(p, extent(r))
	# TODO: Enable further arguments for 'crop'
	imgext <- extract(r, p, fun = fun, df = TRUE, ...)
	# TODO: Test functionality for different options of 'extract'
	colnames(imgext)[colnames(imgext) != "ID"] <- "value"
	if(!missing(id)) {
		colnames(imgext)[colnames(imgext) == "ID"] <- id
		imgext[ , id] <- p@data[ , id]
	}
	return(imgext)
}
