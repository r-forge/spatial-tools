# src_dataset <- system.file("external/tahoe_highrez.tif", package="spatial.tools")

Rgdal_translate <- function(src_dataset,dst_dataset,ot,strict,of,
		b,mask,expand,outsize,scale,unscale,srcwin,projwin,epo,eco,
		a_srs,a_ullr,a_nodata,mo,co,gcp,q,sds,stats,
		additional_commands,
		output_Raster=FALSE,verbose=FALSE)
{
	# Check for gdal installation
	if(is.null(getOption("spatial.tools.gdalInstallation")))
	{
		if(verbose) { message("spatial.tools.gdalInstallation not set, searching for a valid GDAL install (this may take some time)...")}
		gdal_installation <- get_gdal_installation()
	}
	
	gdal_path <- getOption("spatial.tools.gdalInstallation")$gdal_path
	
	# Don't know if this will work on windows yet...
	base_command <- paste("'",file.path(gdal_path,"gdal_translate"),"'",sep="")
	# Don't forget to close the "'" at the end...
	
	
	gdal_flags <- vector()
	# Set up flags
	if(!missing(ot)) { gdal_flags <- paste(gdal_flags,paste("-ot",ot)) }
	if(!missing(strict)) { gdal_flags <- paste(gdal_flags,"-strict") }
	if(!missing(of)) { gdal_flags <- paste(gdal_flags,paste("-of",of)) }
	if(!missing(b)) { gdal_flags <- paste(gdal_flags,paste("-b",b)) }
	if(!missing(mask)) { gdal_flags <- paste(gdal_flags,paste("-mask",mask)) }
	if(!missing(expand)) { gdal_flags <- paste(gdal_flags,paste("-expand",expand)) }
	if(!missing(outsize)) { gdal_flags <- paste(gdal_flags,paste("-outsize",outsize)) }
	if(!missing(scale)) { gdal_flags <- paste(gdal_flags,paste("-scale",scale)) }
	if(!missing(unscale)) { gdal_flags <- paste(gdal_flags,"-unscale") }
	if(!missing(srcwin)) { gdal_flags <- paste(gdal_flags,paste("-srcwin",srcwin)) }
	if(!missing(projwin)) { gdal_flags <- paste(gdal_flags,paste("-projwin",projwin)) }
	if(!missing(epo)) { gdal_flags <- paste(gdal_flags,"-epo") }
	if(!missing(eco)) { gdal_flags <- paste(gdal_flags,"-eco") }
	if(!missing(a_srs)) { gdal_flags <- paste(gdal_flags,paste("-a_srs",a_srs)) }
	if(!missing(a_ullr)) { gdal_flags <- paste(gdal_flags,paste("-a_ullr",a_ullr)) }
	if(!missing(a_nodata)) { gdal_flags <- paste(gdal_flags,paste("-a_nodata",a_nodata)) }
	if(!missing(mo)) { gdal_flags <- paste(gdal_flags,paste("-mo",paste("'",mo,"'",sep=""))) }
	if(!missing(co)) { gdal_flags <- paste(gdal_flags,paste("-co",paste("'",co,"'",sep=""))) }
	if(!missing(gcp)) { gdal_flags <- paste(gdal_flags,paste("-gcp",gcp)) }
	if(!missing(q)) { gdal_flags <- paste(gdal_flags,"-q") }
	if(!missing(sds)) { gdal_flags <- paste(gdal_flags,"-sds") }
	if(!missing(stats)) { gdal_flags <- paste(gdal_flags,"-stats") }
	if(!missing(additional_commands)) { gdal_flags <- paste(gdal_flags,additional_commands) }
	
	
	# Make sure source dataset is available and generate the full path.
	if(dirname(src_dataset)==".") { src_dataset <- file.path(getwd(),src_dataset) }
	if(!file.exists(src_dataset)) { stop(paste("src_dataset:",src_dataset,"does not exist in the system.  Please fix.")) }
	if(dirname(dst_dataset)==".") { dst_dataset <- file.path(getwd(),dst_dataset) }	
	
	gdal_translate_cmd <- paste(paste(base_command,gdal_flags,sep=""),paste("'",src_dataset,"'",sep=""),paste("'",dst_dataset,"'",sep=""))
	
	if(verbose) message(paste("GDAL command being used:",gdal_translate_cmd))
#	print(gdal_command)
	
	if (.Platform$OS=="unix") 
	{
		gdal_translate_output <- system(gdal_translate_cmd,intern=TRUE) 
	} else 
	{
		gdal_translate_output <- shell(gdal_translate_cmd,intern=TRUE)
	}
	
	if(verbose) { message(gdal_translate_output) } 
	
	# (Optional) return Raster
	if(output_Raster)
	{
		return(brick(dst_dataset))	
	} else
	{
		return(NULL)
	}	
}