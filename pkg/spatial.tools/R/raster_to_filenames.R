
raster_to_filenames <- function(x,unique=FALSE)
{
	#TODO: convert this to foreach?
	filenames <- sapply(X=seq(nlayers(x)),
			function(X,raster)
			{
				return(filename(raster(raster,layer=X)))
				
			},			
			raster=x)
	
	if(unique) 
	{
		filenames <- unique(filenames)
	} 
	return(filenames)
	
	
}