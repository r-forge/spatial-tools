crop_stack <- function(x,y,filename="",snap="near",datatype=NULL,...)
{
	if(class(x)=="RasterStack")
	{
		# First we will determine the unique files
		all_filenames <- sapply(x@layers,function(X) {filename(X) } )
		unique_filenames <- unique(all_filenames)
		unique_crops <- 
				foreach(single_filename=unique_filenames,.packages=c("raster")) %dopar%
				{
					crop(brick(single_filename),y,filename,snap,datatype)
				}
		band_layers <- sapply(x@layers,function(x) x@data@band)
		# Put it back together into a stack
		cropped_raster_list <- foreach(
				filenames=all_filenames,
				bands=band_layers,.packages=c("raster")) %dopar%
				{
					file_index <- which(filenames == unique_filenames)
					return(raster(unique_crops[[file_index]],layer=bands))
				}

		cropped_stack <- stack(cropped_raster_list,quick=TRUE)
		return(cropped_stack)
	} else
	{
		return(crop(x,y,filename,snap,datatype,...))
	}
}