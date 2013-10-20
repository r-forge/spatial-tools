#' Spiders a directory for raster files.
#' 
#' @param path Character. The path to search for raster files.
#' @param pattern Character. A regular expression to limit the files that are tested/returned.
#' @param recursive Logical. Search nested subdirectories within the path?
#' @param return_rasters Logical. Return all proper files as RasterBrick objects (TRUE) or as filenames (FALSE).
#' 
#' @return A list of filenames (return_rasters=FALSE) or a list of RasterBricks (return_rasters=TRUE).
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[raster]{brick}},\code{\link[base]{list.files}}
#' 
#' @details This function searches through a path (potentially recursively), and returns
#' the filename and/or a brick for each file that can be coerced safely to a RasterBrick
#' object.  Note that given different flavors of GDAL, this may return different results
#' for the same directory on different computers.  If a foreach parallel backend has
#' been registered, the spidering will use parallel processing to check each file,
#' potentially speeding it up.
#' 
#' @examples { 
#' search_folder <- system.file("external/", package="spatial.tools")
#' # sfQuickInit() # To potentially speed the search up.
#' list.raster.files(path=search_folder)
#' list.raster.files(path=search_folder,return_rasters=TRUE)
#' # sfQuickStop() 
#' }
#' @export

list.raster.files <- function(path=".",pattern=NULL,recursive=FALSE,return_rasters=FALSE)
{
	i <- NULL # To pass R CMD CHECK
	
	# List all files
	all_files <- list.files(path=path,pattern=pattern,recursive=recursive,
			include.dirs=TRUE,full.names=TRUE)
	
	raster_files <- foreach(i=all_files,.packages=c("rgdal","raster")) %dopar%
			{
				safef <- tryCatch(brick(i),error=function(err) return(NULL))
				if(return_rasters)
				{
					if(!is.null(safef)) return(safef) else return(NULL)
				} else
				{
					if(!is.null(safef)) return(i) else return(NULL)
				}
			}

	raster_files <- raster_files[!sapply(raster_files, is.null)]
		
	return(raster_files)
	
}

# search_folder <- system.file("external/", package="spatial.tools")