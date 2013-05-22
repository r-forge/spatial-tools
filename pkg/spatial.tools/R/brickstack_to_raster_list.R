#' @export

brickstack_to_raster_list <- function(x)
{
	nlayers_x <- nlayers(x)
	raster_list <- foreach(layer=1:nlayers_x,.packages=c("raster")) %dopar%
		raster(x,layer=layer)
	
	return(raster_list)
	
}