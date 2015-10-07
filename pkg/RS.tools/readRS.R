# LEDAPS LANDSAT CLIMATE DATA RECORD (CDR) SURFACE REFLECTANCE 
# Landsat 4-5 TM, 7 ETM+
lndsr.driver <- function(infile,outfile,of="GTiff",spectral_only=FALSE,output_Raster=FALSE,verbose=FALSE,...)
{
	infile <- normalizePath(infile)
	
	if(missing(outfile))
	{
		if(of=="GTiff") suffix=".tif"
		outfile <- paste(remove_file_extension(infile),suffix,sep="")
	}
	
	layer_names <- sapply(get_subdatasets(infile),
			function(x)
			{
				return(strsplit(x,":")[[1]][5])
			
			})
	
	nlayers = length(layer_names)
	
	layer_indices <- seq(layer_names)
	
	if(spectral_only)
	{
		layer_indices <- layer_indices[layer_names %in% c("band1","band2","band3","band4","band5","band7","band6")]
	}
	
	layer_names_full <- paste(outfile,"_",layer_names,sep="")[layer_indices]
	
	# Extract each layer (this only works with GDAL > 1.10, need to fix this for lower versions)
	# NOT WORKING
#	layer_vrts <- foreach(layer=layer_indices,.packages="gdalUtils",.combine=rbind) %dopar%
#			{
#				tempvrt <- paste(tempfile(),".vrt",sep="")
#				gdalbuildvrt(infile,tempvrt,sd=layer)
#				return(tempvrt)
#			}
	
	
		layer_tiffs <- foreach(layer=layer_indices,.packages="gdalUtils",.combine=rbind) %dopar%
			{
				temptiff <- paste(tempfile(),".tif",sep="")
				gdal_translate(infile,temptiff,sd_index=layer,verbose=verbose)
				return(temptiff)
			}
	
	# Combine layers:
	tempvrt <- paste(tempfile(),".vrt",sep="")
	gdalbuildvrt(layer_tiffs,tempvrt,separate=TRUE,verbose=verbose)
	gdal_translate(tempvrt,outfile,of=of,verbose=verbose)
	if(output_Raster)
	{
		out <- brick(outfile)
		names(out) <- layer_names[layer_indices]
		return(out)
	} else
	{
		return(outfile)
	}
}