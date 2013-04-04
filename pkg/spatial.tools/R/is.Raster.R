

is.Raster <- function(x)
{
	return((class(x)=="RasterLayer" || class(x)=="RasterBrick" || class(x)=="RasterStack"))
}