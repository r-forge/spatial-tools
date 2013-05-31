#' @export

modify_raster_margins <- function(x,extent_delta=c(0,0,0,0),value=NA)
{
	x_extents <- extent(x)
	res_x <- res(x)
	
	x_modified <- x
	
	if(any(extent_delta < 0))
	{
		# Need to crop
		# ul:
		ul_mod <- extent_delta[c(1,3)] * res_x
		ul_mod[ul_mod > 0] <- 0
		lr_mod <- extent_delta[c(2,4)] * res_x
		lr_mod[lr_mod > 0] <- 0
		
		crop_extent <- as.vector(x_extents)
		crop_extent[c(1,3)] <- crop_extent[c(1,3)] - ul_mod
		crop_extent[c(2,4)] <- crop_extent[c(2,4)] + lr_mod
		
		x_modified <- crop(x_modified,crop_extent)
	}
	
	if(any(extent_delta > 0))
	{
		# Need to crop
		# ul:
		ul_mod <- extent_delta[c(1,3)] * res_x
		ul_mod[ul_mod < 0] <- 0
		lr_mod <- extent_delta[c(2,4)] * res_x
		lr_mod[lr_mod < 0] <- 0
		
		extend_extent <- as.vector(x_extents)
		extend_extent[c(1,3)] <- extend_extent[c(1,3)] - ul_mod
		extend_extent[c(2,4)] <- extend_extent[c(2,4)] + lr_mod
		
		x_modified <- extend(x_modified,extend_extent,value=value)
	}
	
	return(x_modified)
}