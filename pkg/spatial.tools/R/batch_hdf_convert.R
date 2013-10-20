
batch_hdf_convert <- function(input_directory,output_directory,recursive=TRUE,gdal_path,layers,
		format)
{
	setwd(input_directory)
	hdf_files <- list.files(path=input_directory,pattern=glob2rx("*.hdf"),recursive=recursive,full.names=TRUE)
	if(length(hdf_files)==0) stop("No hdf files found.")
	
	setwd(output_directory)
	
	gdal_translate <- normalizePath(file.path(gdal_path,list.files(path=gdal_path,pattern=glob2rx("gdal_translate*"))[1]))
	gdalbuildvrt <- normalizePath(file.path(gdal_path,list.files(path=gdal_path,pattern=glob2rx("gdalbuildvrt*"))[1]))
			
	file_basenames <- sapply(hdf_files,function(x) strsplit(basename(hdf_file),".hdf")[[1]])
	
	output_stacks <- foreach(hdf_file in hdf_files, file_basename in file_basenames) %dopar%
			{
				# Should save this to temp dir
				setwd(output_directory)
				#	file_basename <- strsplit(basename(hdf_file),".hdf")[[1]]
				dst_file <- file.path(output_directory,file_basename)
				cmd <- paste('"',gdal_translate,'" -sds ','"',hdf_file,'" ','"',dst_file,'"',sep="")
				gdal_translate_out <- system(cmd,intern=TRUE)
				output_files <- dir(pattern=glob2rx(paste(file_basename,"*",sep="")))
				if(!missing(layers)) output_files <- output_files[layers]
				output_stack <- stack(output_files)
			}
	
	if(!missing(outformat))
	{
		out_translate <- foreach(output_stack in output_stacks,
						file_basename in file_basenames,
						.packages="raster") %dopar%
				{
					setwd(output_directory)
					dst_file <- paste(file.path(output_directory,file_basename),".vrt",sep="")
					src_files <- normalizePath(list.files(pattern=file_basename,full.names=TRUE))
				#	outbrick <- writeRaster(x=output_stack,filename=file_basename,format=format)			
					cmd <- paste('"',gdalbuildvrt,'" -separate ','"',dst_file,
						'" ','"',paste('"',src_files,'" ',collapse=""),'"',sep="")
					gdalbuildvrt_out <- system(cmd,intern=TRUE)
					
				}
	}
}