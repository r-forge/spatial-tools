
rasterEngine_prestack <- function(x,...,quick=TRUE,verbose=FALSE)
{
	additional_vars <- list(...)
	additional_vars_isRaster <- sapply(additional_vars,is.Raster)
	additional_vars_Raster <- additional_vars[additional_vars_isRaster]
	
	if(verbose) { message("Prestacking inputs...") }
	if(missing(x))
	{
		#	x <- stack(additional_vars_Raster,quick=quick)
		additional_vars_Raster_names <- names(additional_vars_Raster)
		names(additional_vars_Raster) <- NULL
		names(additional_vars_Raster)[1] <- "x"
		x <- do.call(stack,c(additional_vars_Raster,quick=quick))
		
		nlayers_Rasters <- sapply(additional_vars_Raster,nlayers)
		nlayers_indices <- unlist(mapply(function(varname,nlayers) { rep(varname,nlayers) },
						varname=additional_vars_Raster_names,nlayers=nlayers_Rasters))
	} else
	{
		x <- stack(x,additional_vars_Raster,quick=quick)
		nlayers_Rasters <- c(nlayers(x),sapply(additional_vars_Raster,nlayers))
		nlayers_indices <- unlist(mapply(function(varname,nlayers) { rep(varname,nlayers) },
						varname=c("x",names(additional_vars_Raster)),nlayers=nlayers_Rasters))
	}
	
	if(verbose) { message("Finished prestacking inputs...") }
	
	return(x)
	
}