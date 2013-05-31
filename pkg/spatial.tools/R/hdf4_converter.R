hdf4_converter <- function(
		src_dataset,
		dst_dataset,format,
		gdal_path=NULL,
		subdatasets=NULL,
		file_extension=NULL,
		return_raster=TRUE,
		verbose=FALSE)
{
	if(is.null(gdal_path))
	{
		if(verbose) message("Searching for GDAL (set gdal_path for faster execution)...")
		gdal_installs <- get_gdal_installation(check_for_drivers="HDF4")
		if(length(gdal_installs)==0) 
		{stop("You need a GDAL that has the HDF4 driver.  
							If you are using Windows, try installing OSGEO4W or FWTools.  
							?get_gdal_installation for more info.")
		} else
		{
			latest_gdal_version_ranking <- 
					order(c(gdal_installs[[1]]$version,gdal_installs[[2]]$version),decreasing=TRUE)
			latest_gdal_index <- which(latest_gdal_version_ranking==1)
			if(verbose) message(paste("Using ",gdal_installs[[latest_gdal_index]]$gdal_path,sep=""))
			gdal_path <- gdal_installs[[latest_gdal_index]]$gdal_path
		}
	}
	
	gdal_translate_path <- 
			paste('"',
			file.path(gdal_path,list.files(path=gdal_path,pattern=glob2rx("gdal_translate*"))[1]),
			'"',sep="")
#	if(dirname(src_dataset)==".")	
#	{ 
#		src_dataset_fullpath <- file.path(getwd(),src_dataset) 
#	} else 
#	{ 
		src_dataset_fullpath <- paste('"',src_dataset,'"',sep="") 
#	}
	
	if(dirname(dst_dataset)==".")	
	{ 
		dst_dataset_fullpath <- file.path(getwd(),dst_dataset) 
	} else 
	{ 
		dst_dataset_fullpath <- dst_dataset 
	}
	
	# Flags here:
	gdal_flags <- vector()
	gdal_flags <- c(gdal_flags,paste("-of",format))
	
	gdal_translate_cmd <- paste(
		gdal_translate_path,
		gdal_flags,
		src_dataset_fullpath,
		dst_dataset
		)
	
	gdal_translate_dump <- shell(gdal_translate_cmd,intern=TRUE)
		
	
}


"c:/Program Files (x86)/Quantum GIS Lisboa/bin/gdal_translate.exe" -of GTiff "HDF4_EOS:EOS_GRID:P:/housing_bubble/scratch/test_ModisDownload/MOD13Q1.A2001001.h10v10.005.2008270004349.hdf:MODIS_Grid_16DAY_250m_500m_VI:250m 16 days NDVI" mooR.tif
