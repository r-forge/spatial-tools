
gdalinfo_spider <- function(root_directory=".",pattern=NULL,recursive=FALSE,
		return_bbox=FALSE,spider_rasters=TRUE)
{
	# Needs plyr, rgdal, foreach, raster
	
	all_files <- list.files(path=root_directory,pattern=pattern,recursive=recursive,
			full.names=TRUE)
	
	if(spider_rasters)
	{
		files_GDALinfo_raw <- foreach(i=all_files,.packages=c("rgdal")) %dopar%
				{
					f <- function(i) GDALinfo(i,silent=TRUE)
					safef <- failwith(NULL,f)
					return(safef(i))
				}
		files_GDALinfo_index <- sapply(files_GDALinfo_raw,function(X) { !is.null(X) })
		files_GDALinfo <- files_GDALinfo_raw[files_GDALinfo_index]
	}
	
# 	We will need to write these one-by-one
#	if(spider_vectors)
#	{
#		files_ogrInfo_raw <- foreach(i=all_files,.packages=c("rgdal")) %dopar%
#				{
#					f <- function(i) ogrInfo(dsn=".",layer=i)
#					safef <- failwith(NULL,f)
#					return(safef(i))
#				}
#	}
	
	
	if(return_bbox)
	{
		GDALinfo_files <- all_files[files_GDALinfo_index]
		files_GDALinfo_bbox <- foreach(i=GDALinfo_files,.packages=c("rgdal")) %dopar%
				{
					temp_brick=brick(i)
				}
	}
	
}
root_directory <- "X:\\private\\code\\eclipse_workspaces\\GEOG-PC53\\geog489-s13"
pattern=NULL
recursive=TRUE