#' Performs a performs area-weighted resampling of raster datasets.
#' 
#' @param from Raster* The sources raster to be resampled.
#' @param to Raster* A target raster that the from will be resampled to (extent, resolution, projection).
#' @param method Character. Default is "mode". See details.
#' @param verbose logical. Enable verbose execution? Default is FALSE.  
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[raster]{projectRaster}}, \code{\link[raster]{extract}}, \code{\link[raster]{aggregate}}
#' @details This function is designed to solve the problem of resampling/reprojecting rasters
#' using area-based, not point based (e.g. nearest neighbor, bilinear, cubic convolution), 
#' resampling.  The output pixel is a function of the areas of the input pixels, so this
#' should be used for resampling from a finer resolution to a coarser resolution.
#' 
#' The method defaults to "mode", which will return the value covering the largest area
#' of the output pixel area.  Other methods will be added in the future.
#' 
#' A word of warning: this algorithm is SLOW.  The function uses focal_hpc, so we 
#' highly recommend using it with a foreach engine running (e.g. use sfQuickInit() ).
#' Keep in mind this is a "dirty" parallel problem, so different chunks execute at
#' different speeds.
#' 
#' @export

projectRaster_areal <- function(from,to,method="mode",verbose=FALSE)
{
	chunk_function <- function(x,from,method,...)
	{
		chunk_vector <- rasterToPolygons(x,na.rm=FALSE,n=16)
		chunk_vector_reproject <- spTransform(chunk_vector,CRS(projection(from)))
		chunk_vector_extract <- extract(from,chunk_vector_reproject,weights=TRUE,na.rm=FALSE)
		if(method=="mode")
		{
			chunk_vector_extract_area <- 
					sapply(chunk_vector_extract,
							function(x)
							{
								sum_class<-tapply(x[,2],x[,1],sum)
								class_names <- names(sum_class)
								return(as.numeric(class_names[which.max(sum_class)]))
							}
					)
		}
		return(array(chunk_vector_extract_area,dim=c(dim(x)[2],dim(x)[1],1)))
	}
	return(focal_hpc(x=raster(to,layer=1),fun=chunk_function,args=list(from=from,method=method),
					chunk_format="raster",verbose=verbose))
}
