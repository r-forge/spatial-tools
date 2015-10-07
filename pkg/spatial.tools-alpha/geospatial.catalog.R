

geospatial.catalog <- function(path=".",
		catalog.rasters=TRUE,
		catalog.vectors=TRUE,
		raster.pattern=NULL,
		vector.pattern=NULL,
		recursive=FALSE,info_only=TRUE,
		verbose=FALSE)
{
	if(verbose) message("Initial file scan...")
	
	# List every file:
	all_files <- list.files(path=path,recursive=recursive,
			include.dirs=TRUE,full.names=TRUE)
	
#	all_files <- normalizePath(all_files)
	
	# Raster catalog
	if(catalog.rasters)
	{
		if(verbose) message ("Cataloging rasters...")
		
		potential_rasters_ids <- grep(all_files,pattern=raster.pattern)
		potential_raster_files <- all_files[potential_rasters_ids]
		
		# First, we have to brick up each raster to confirm it is "real".
		catalog.bricks <- foreach(fileid = seq(potential_raster_files),
						.packages=c("raster")) %do%
				{
					if(verbose) message(fileid)
					system.file(
					safef <- tryCatch(brick(
									potential_raster_files[fileid]
									),error=function(err) return(NULL))
					)
					print(safef)
					if(!is.null(safef)) return(safef) else return(NULL)
				}
		
		
	}
	
	
	
}

path="P:/cali_disturbance/raster/landsat/raw/"
catalog.vectors=FALSE
catalog.rasters=TRUE
recursive=TRUE
raster.pattern=".tif"