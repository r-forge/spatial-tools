# # examples
#tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools"))
#tahoe_lidar_highesthit <- raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools"))
#tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
#' @export

rasterEngine <- function(x,
		fun=NULL,args=NULL, 
		window_dims=c(1,1), 
		window_center=c(ceiling(window_dims[1]/2),ceiling(window_dims[2]/2)),
		filename=NULL, overwrite=FALSE,outformat="raster",
		chunk_format="array",minblocks="max",blocksize=NULL,
		quick=TRUE,verbose=FALSE,...) 
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
	#	r_check_function <- do.call(fun, r_check_args)

	focal_hpc_multiRaster_function <- function(x,nlayers_indices,fun,...)
	{
		varnames <- unique(nlayers_indices)
		# Create a list of variables
		function_vars <- sapply(X=varnames,
			FUN=function(X,nlayers_indices,x) 
			{
				var_index <- which(nlayers_indices==X)
				var_sub <- x[,,var_index,drop=FALSE]
				return(var_sub)
			},nlayers_indices=nlayers_indices,
			x=x,
			simplify=FALSE)
		function_vars <- c(function_vars,list(...))
		out <- do.call(fun,function_vars)
		dim(out) <- c(dim(x)[1:2],(length(out)/prod(dim(x)[1:2])))
		return(out)
	}
	
	focal_hpc(x,fun=focal_hpc_multiRaster_function,args=c(list(nlayers_indices=nlayers_indices,fun=fun),args),
			window_dims=window_dims, 
			window_center=window_center,
			filename=filename, overwrite=overwrite,outformat=outformat,
			chunk_format=chunk_format,minblocks=minblocks,blocksize=blocksize,
			verbose=verbose)
}

#height_function <- function(firstreturn,bareearth,offsetmoo,...)
#{
#	return((bareearth-firstreturn+offsetmoo))
##	return(as.vector(bareearth-firstreturn))
#}
##
### debug(focal_hpc_multiRaster)
#height_diff <- focal_hpc_multiRaster(firstreturn=tahoe_lidar_highesthit,bareearth=tahoe_lidar_bareearth,fun=height_function,args=list(offsetmoo=100),verbose=TRUE)
##
#height_diff_nohpc <- tahoe_lidar_bareearth - tahoe_lidar_highesthit