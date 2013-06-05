 # examples
tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools"))
tahoe_lidar_highesthit <- raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools"))
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))

focal_hpc_multiRaster <- function(x,
		fun,args=NULL, 
		window_dims=c(1,1), 
		window_center=c(ceiling(window_dims[1]/2),ceiling(window_dims[2]/2)),
		filename=NULL, overwrite=FALSE,outformat="raster",
		chunk_format="array",minblocks="max",blocksize=NULL,
		quick=TRUE,verbose=FALSE,...) 
{
	additional_vars <- list(...)
	additional_vars_isRaster <- sapply(additional_vars,is.Raster)
	additional_vars_Raster <- additional_vars[additional_vars_isRaster]
	if(missing(x))
	{
		x <- stack(additional_vars_Raster,quick=quick)
		nlayers_Rasters <- sapply(additional_vars_Raster,nlayers)
		nlayers_indices <- unlist(mapply(function(varname,nlayers) { rep(varname,nlayers) },
				varname=names(additional_vars_Raster),nlayers=nlayers_Rasters))
	} else
	{
		x <- stack(x,additional_vars_Raster,quick=quick)
		nlayers_Rasters <- c(nlayers(x),sapply(additional_vars_Raster,nlayers))
		nlayers_indices <- unlist(mapply(function(varname,nlayers) { rep(varname,nlayers) },
			varname=c("x",names(additional_vars_Raster)),nlayers=nlayers_Rasters))
	}

	#	r_check_function <- do.call(fun, r_check_args)

	focal_hpc_multiRaster_function <- function(x,nlayers_indices,fun,...)
	{
		varnames <- unique(nlayers_indices)
		# Create a list of variables
		function_vars <- sapply(varnames,
			function(X,nlayers_indices)
			{
				
				
				
			},nlayers_indices=nlayers_indices,x=x,
			simplify=FALSE)
		
		
	}
	
	
	return(NULL)
}

height_function <- function(firstreturn,bareearth,...)
{
	return(bareearth-firstreturn)
}

debug(focal_hpc_multiRaster)
focal_hpc_multiRaster(firstreturn=tahoe_lidar_highesthit,bareearth=tahoe_highrez,fun=height_function)