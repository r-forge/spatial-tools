# Cribbed from the MODIS package with mods for spatial.tools

get_gdal_installation=function(quiet=FALSE,return_drivers=TRUE,return_versions=TRUE,
		check_for_drivers,check_for_python_utilities=TRUE)
{
	if (.Platform$OS=="unix")
	{    
		# Need to fix this
		if (!quiet)
		{
			cat("Checking availabillity of GDAL:\n")
		}
		gdal <- try(system("gdalinfo --version",intern=TRUE),silent=TRUE)
		if (inherits(gdal,"try-error"))
		{
			cat("   GDAL not found, install it or check path settings in order to use related functionalities!\n")
			gdal <- "Could not determine GDAL version!"
		} else 
		{
			if (!quiet)
			{
				cat("   OK,",gdal,"found!\n")
			}
			GDAL <- TRUE
		}
		GDAL <- list(GDAL=GDAL,version=gdal)
		
	} else 
	{		
		cmd <- 'gdalinfo --version'
		gdal_paths=dirname(list.files(path="c:/",pattern="^gdalinfo.exe$", full.names=TRUE, recursive=TRUE,include.dirs=TRUE))
		
		gdal_installation_list=vector(mode="list",length=length(gdal_paths))
		
		current_dir=getwd()
		
		for(i in 1:length(gdal_paths))
		{
			setwd(gdal_paths[i])
			gdal_installation_list[[i]]$gdal_path=getwd()
			gdal <- shell(cmd,intern=TRUE)
			version=strsplit(strsplit(gdal,",")[[1]][1]," ")[[1]][2]
			
			gdal_installation_list[[i]]$version=version
			if(return_drivers)
			{
				drivers_raw=shell("gdalinfo --formats",intern=TRUE)
				drivers=strsplit(drivers_raw,":")
				driver_names=gsub("^ ","",sapply(drivers,function(x) { x[2] })) # Need to remove spaces
				driver_codes_perm=strsplit(sapply(drivers,function(x) { x[1] }),"\\(")
				driver_codes=gsub(" ","",sapply(driver_codes_perm,function(x) { x[1] }),fixed=TRUE)
				driver_perm=gsub("\\)","",sapply(driver_codes_perm,function(x) { x[2] }))
				drivers_dataframe=data.frame(format_code=driver_codes,format_rw=driver_perm,format_name=driver_names)
				
				drivers_dataframe=drivers_dataframe[2:dim(drivers_dataframe)[1],]
				gdal_installation_list[[i]]$drivers=drivers_dataframe
			}
			if(check_for_python_utilities)
			{
				gdal_installation_list[[i]]$python_utilities <- dir(pattern=".py") 
					# file.exists("gdal_polygonize.py")
			}
		}
		
		setwd(current_dir)
		
	}
	
	if(!missing(check_for_drivers))
	{
		format_checked=vector(mode="logical",length=length(gdal_installation_list))
		check_for_drivers_n=length(check_for_drivers)
		for(i in 1:length(gdal_installation_list))
		{
			check=check_for_drivers %in% gdal_installation_list[[i]]$drivers$format_code
			format_checked[i]=sum(check)==check_for_drivers_n
		}
		if(sum(format_checked)==0)
		{
			print("No GDAL installations match those drivers...")
			return(NULL)
		} else
		{
			gdal_installation_list=gdal_installation_list[format_checked]
		}
	}
	
	return(gdal_installation_list)
}