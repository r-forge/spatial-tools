create_blank_raster <- function(filename=NULL,
	format="raster",dataType="FLT8S",bandorder="BSQ",
	nrow,ncol,nlayers,
	create_header=TRUE,reference_raster=reference_raster,
	overwrite=FALSE,verbose=FALSE)
{
	# Setup blank file.
	outdata_ncells=as.numeric(nrow)*as.numeric(ncol)*as.numeric(nlayers)
	if(verbose) cat("outdata_ncells=",outdata_ncells,"\n")
	if(is.null(filename))
	{	
		filename <- tempfile()
		if(verbose) { print(paste("No output file given, using a tempfile name:",filename,sep=" ")) }
		if(!file.exists(tempdir())) dir.create(tempdir())
	} 
	
	if(dataType=="FLT8S") numBytes = 8
	
	# I've been warned about using seek on Windows, but this appears to work...
	if(verbose) { print("Creating empty file.") }
	out=file(filename,"wb")
	seek(out,(outdata_ncells-1)*numBytes)
	writeBin(raw(numBytes),out)
	close(out)
	
	if(create_header)
	{
		# Setup header.
		if(verbose) { print("Setting up output header.") }
		if(nlayers > 1) 
		{ 
			reference_raster=brick(raster(reference_raster,layer=1),nl=nlayers) 
		} else
		{
			if(nlayers(reference_raster) > 1) { 
				reference_raster=raster(reference_raster,layer=1) 
			} 	
		}
		
		if(format=="raster") { 
			if(verbose) { print("Outformat is raster.  Appending .gri to filename.") }
			file.rename(filename,paste(filename,".gri",sep=""))
			filename=paste(filename,".gri",sep="")
		}
		
		outraster <- build_raster_header(x_filename=filename,
				reference_raster=reference_raster,
				out_nlayers=nlayers,dataType=dataType,format=format,
				bandorder=bandorder)
	}
	return(filename)
}