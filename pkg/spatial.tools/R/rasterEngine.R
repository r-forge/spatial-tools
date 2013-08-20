#' @export

rasterEngine <- function(x,
		fun=NULL,args=NULL, 
		window_dims=c(1,1), 
		window_center=c(ceiling(window_dims[1]/2),ceiling(window_dims[2]/2)),
		filename=NULL, overwrite=FALSE,outformat="raster",
		chunk_format="array",minblocks="max",blocksize=NULL,
		prestack=NULL,
		quick=TRUE,outbands=NULL,
		processing_unit=NA,
		verbose=FALSE,...) 
{
	additional_vars <- list(...)
	additional_vars_isRaster <- sapply(additional_vars,is.Raster)
	additional_vars_Raster <- additional_vars[additional_vars_isRaster]
	
	# Need to add processing unit processing_unit
	if(is.na(processing_unit))
	{
		if(sum(window_dims) > 2)
		{
			processing_unit="single"
		} else
		{
			processing_unit="chunk"
		}
	}
	
	
	if(missing(x))
	{
		x <- additional_vars_Raster
	} else
	{
		x <- c(x,additional_vars_Raster)
		names(x)[[1]] <- "x"
	}
	
	focal_hpc_multiRaster_function <- function(x,fun,...)
	{
		function_vars <- c(x,list(...))
		out <- do.call(fun,function_vars)
		return(out)
	}
	
	focal_hpc(x,fun=focal_hpc_multiRaster_function,args=c(list(fun=fun),args),
			window_dims=window_dims, 
			window_center=window_center,
			filename=filename, overwrite=overwrite,outformat=outformat,
			chunk_format=chunk_format,minblocks=minblocks,blocksize=blocksize,
			outbands=outbands,
			processing_unit=processing_unit,
			verbose=verbose)
}