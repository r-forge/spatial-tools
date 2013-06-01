

modis_hdf4_subdatasets <- function(x,gdal_path,verbose=FALSE)
{
	
	if(is.null(gdal_path))
	{
		if(verbose) message("Searching for GDAL (set gdal_path for faster execution)...")
		gdal_installs <- get_gdal_installation(required_drivers="HDF4")
		if(length(gdal_installs)==0) 
		{stop("You need a GDAL that has the HDF4 driver.  
							If you are using Windows, try installing OSGEO4W or FWTools.  
							?get_gdal_installation for more info.")
		} else
		{
			if(verbose) message(paste("Using ",gdal_installs[[1]]$gdal_path,sep=""))
			gdal_path <- gdal_installs[[1]]$gdal_path
		}
	}
	
#	current_dir <- getwd()
#	setwd(gdal_path)
	gdalinfo_path <- file.path(gdal_path,list.files(path=gdal_path,pattern=glob2rx("gdalinfo*"))[1])
	if(dirname(x)==".")	{ x_fullpath <- file.path(getwd(),x) } else x_fullpath <- x
	cmd <- paste("\"",gdalinfo_path,"\" ",x_fullpath,sep="")
	gdalinfo_dump <- shell(cmd,intern=TRUE)
	subdataset_rawnames <- gdalinfo_dump[grep(glob2rx("*SUBDATASET*NAME*"),gdalinfo_dump)]
	
	subdataset_names <- sapply(X=seq(length(subdataset_rawnames)),FUN=function(X)
			{
				split1 <- strsplit(subdataset_rawnames[X],"=")
				return(gsub("\"","",split1[[1]][2]))
				
			})
	
	return(subdataset_names)
}