# http://menugget.blogspot.com/2012/03/xyz-geographic-data-interpolation-part.html

ncep_to_raster <- function(ncep,to)
{
	# First read the ncep
	ncep_brick <- brick(ncep)
	
	# Convert to points
	ncep_points <- rasterToPoints(ncep_brick,spatial=TRUE)
	
}