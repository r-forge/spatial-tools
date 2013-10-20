list.raster.files <- function(path=".",pattern=NULL,recursive=FALSE,return_rasters=TRUE)
{
	# List all files
	all_files <- list.files(path=path,pattern=pattern,recursive=recursive,
			include.dirs=TRUE,full.names=TRUE)
	
	raster_files <- foreach(i=all_files,.packages=c("raster")) %dopar%
			{
				#	f <- function(i) GDALinfo(i,silent=TRUE)
				# f <- function(i) brick(i)
				safef <- tryCatch(brick(i),error=function(err) return(NULL))
				#		safef <- failwith(NULL,f)
				if(return_rasters)
				{
					if(!is.null(safef)) return(safef) else return(NULL)
				} else
				{
					if(!is.null(safef)) return(i) else return(NULL)
				}
				#			return(safef(i))
			}
	# For some reason, we're getting NULL in the output:
	raster_files <- raster_files[!sapply(raster_files, is.null)]
	
	return(raster_files)
	
}

