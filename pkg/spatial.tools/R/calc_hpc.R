#' Performs raster calculations using a snowfall cluster.
#' @title calc_hpc
#' @param x Raster*. A Raster* used as the input into the function.  Multiple inputs should be stacked together.
#' @param fun function. A function to be applied to the image. The input and outputs of the function should be a vector or matrix (see Details).
#' @param args list. Arguments to pass to the function (see ?mapply).
#' @param filename character. Filename of the output raster.
#' @param cl cluster. A cluster object. calc_hpc will attempt to determine this if missing.
#' @param disable_cl logical. Disable parallel computing? Default is FALSE. 
#' @param outformat character. Outformat of the raster. Must be a format usable by hdr(). Default is 'raster'.
#' @param overwrite logical. Allow files to be overwritten? Default is FALSE.
#' @param verbose logical. Enable verbose execution? Default is FALSE.  
#' @author Jonathan A. Greenberg, Pritam Sukumar, and Robert Hijimans (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{dataType}} \code{\link{hdr}} \code{\link{mmap}} \code{\link{clusterMap}}
#' @details calc_hpc is designed to execute a function on a Raster* object using a snowfall cluster 
#' to achieve parallel reads, executions and writes. Parallel random writes are achieved through the use of
#' mmap, so individual image chunks can finish and write their outputs without having to wait for
#' all nodes in the cluster to finish and then perform sequential writing.  On Windows systems,
#' random writes are possible but apparently not parallel writes.  calc_hpc solves this by trying to
#' write to a portion of the image file, and if it finds an error (a race condition occurs), it will
#' simply retry the writes until it successfully finishes.  On a Linux system, truly parallel writes
#' should be possible.
#' 
#' The functions can be arbitrarily complex, but should always rely on a vector (if a single band input)
#' or a matrix (if a multiband input).  getValues() is used to read a chunk of data that is passed to the 
#' function.  If the function relies on multiple, same-sized raster/brick inputs, they should first
#' be coerced into a single stack(), and then the beginning of the function should deconstruct the
#' stack (which will be received by the function as a matrix) back into individual components 
#' (see the example below).  
#' 
#' The speed of the execution will vary based on the specific setup, and may, indeed, be slower than
#' a sequential execution (e.g. with calc() ).
#' @examples
#' sfInit(parallel=TRUE,cpus=1)
#' tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
#' ndvi_function <- function(x,red_band,NIR_band) { 
#' 	red=x[,red_band]
#' 	NIR=x[,NIR_band]
#' 	ndvi = (NIR-red)/(NIR+red)
#' 	return(ndvi)
#' }
#' ndvi_args=list(red_band=2,NIR_band=3)
#' # Sequential execution.
#' system.time(calc_hpc(x=tahoe_highrez,fun=ndvi_function,args=ndvi_args,
#' 	overwrite=TRUE,filename="testndvi3",disable_cl=TRUE,verbose=FALSE))
#' # Parallel execution.
#' system.time(calc_hpc(x=tahoe_highrez,fun=ndvi_function,args=ndvi_args,
#' 	overwrite=TRUE,filename="testndvi3",disable_cl=FALSE,verbose=FALSE))
#' sfStop()
#' @export

calc_hpc <- function(x, fun, args=NULL, filename='', cl=NULL, disable_cl=FALSE,
		overwrite=FALSE,outformat='raster',verbose=FALSE) {
	require("raster")
	require("snowfall")
	require("rgdal")
	require("mmap")

	# Do some file checks up here.
	
#	if(verbose)
#	{
#		print(args)
#	}
	
	if(disable_cl)
	{
		if(verbose) { print("Cluster disabled.") }
		nodes <- 1
	} else
	{
		if(verbose) { print("Cluster enabled, loading packages.") }
		if (is.null(cl)) {
			cl <- sfGetCluster()
			sfLibrary("snowfall",character.only=TRUE)
			sfLibrary("rgdal",character.only=TRUE)
			sfLibrary("raster",character.only=TRUE)
			sfLibrary("mmap",character.only=TRUE)
		}
		nodes <- sfNodes()
	}
	# We should test a single pixel here to see the size of the output...
	
	# We are going to pull out the first row and first two pixels to check the function...
	if(verbose) { print("Checking the function on a small chunk of data.") }
	r_check <- getValues(crop(x, extent(x, r1=1, r2=1, c1=1,c2=2)))
	
	if(!is.null(args)) {
#		r_check_function <- getValues(do.call(fun, c(r_check, args)))
		r_check_args=args
		r_check_args$x=r_check
		r_check_function <- do.call(fun, r_check_args)
	} else
	{
#		r_check_function <- getValues(fun(r_check))
		r_check_function <- fun(r_check)
	}
	
	if(class(r_check_function)=="numeric")
	{
		outbands=1
	} else
	{
		outbands=dim(r_check_function)[2]
	}
	
	if(verbose) { print(paste("Number of output bands determined to be:",outbands,sep=" ")) }
	
	# Without the as.numeric some big files will have an integer overflow
	outdata_ncells=as.numeric(nrow(x))*as.numeric(ncol(x))*as.numeric(outbands)
	if(filename=="")
	{	
		filename <- tempfile()
		if(verbose) { print(paste("No output file given, using a tempfile name:",filename,sep=" ")) }
	} 
	
	# I've been warned about using seek on Windows, but this appears to work...
	if(verbose) { print("Creating empty file.") }
	out=file(filename,"wb")
	seek(out,(outdata_ncells-1)*8)
	writeBin(raw(8),out)
	close(out)
	
#	m <- max(1, round(m))
  
	tr <- blockSize(x, minblocks=1 )
	if (tr$n < nodes) {
		nodes <- tr$n
	}
	
	if(verbose) { print(paste("Number of chunks:",tr$n,sep=" ")) }
	
	tr$row2 <- tr$row + tr$nrows - 1

	tr_out=list(row=((tr$row-1)*outbands+1))
	tr_out$row2=((tr$row2)*outbands)
	
	i=1:tr$n

	if(verbose) { print("Loading chunk function.") }
	chunkFunction <- function(fun,i,args,x,tr,filename,outbands,verbose) 
	{
#		sfCat(print(i))
		r <- getValues(crop(x, extent(x, r1=tr$row[i], r2=tr$row2[i], c1=1, c2=ncol(x))))
		if(is.null(args)) {
			r <- fun(r) 
		} else
		{
			fun_args=args
			fun_args$x=r
			r_out <- do.call(fun, fun_args)
		}	
		print(class(r_out))
		print(dim(r_out))
		cellStart=((cellFromRowCol(x,rownr=tr$row[i],colnr=1))-1)*outbands+1
		cellEnd=((cellFromRowCol(x,rownr=tr$row2[i],colnr=ncol(x))))*outbands

		parallel_write <- function(filename,r_out,cellStart,cellEnd)
		{
			out <- mmap(filename, mode=real64())
			out[cellStart:cellEnd] <- as.vector(r_out)
			munmap(out)
		}
		
		writeSuccess=FALSE
		while(!writeSuccess)
		{
			writeSuccess=TRUE
			tryCatch(parallel_write(filename,r_out,cellStart,cellEnd),
				error=function(err) writeSuccess <<- FALSE)	
		}
	}
	
	if(verbose) { print("Loading chunk arguments.") }
	chunkArgs <- list(fun=fun,x=x,tr=tr,args=args,filename=filename,
		outbands=outbands,verbose=verbose)
	
	if(disable_cl)
	{
		if(verbose) { print("Sequential computation started.") }
		if(verbose) { start_time = proc.time() }
		mapply(chunkFunction,i,MoreArgs=chunkArgs)
		if(verbose) { print("Sequential computation finished.  Timing:") }
		if(verbose) { print(proc.time()-start_time) }
	} else
	{
		if(verbose) { print("Parallel computation started.") }
		if(verbose) { start_time = proc.time() }
		clusterMap(cl,chunkFunction,i,MoreArgs=chunkArgs)
		if(verbose) { print("Parallel computation finished.  Timing:") }
		if(verbose) { print(proc.time()-start_time) }
	}

	# Let's see if we can trick raster into making us a proper header
	if(verbose) { print("Setting up output header.") }
	if(outbands > 1) 
	{ 
		reference_raster=brick(raster(x,layer=1),nl=outbands) 
	} else
	{
		if(nlayers(x) > 1) { reference_raster=raster(x,layer=1) } else
		{ reference_raster=x }	
	}
	
	if(outformat=='raster') { 
		if(verbose) { print("Outformat is raster.  Appending .gri to filename.") }
		file.rename(filename,paste(filename,".gri",sep=""))
		filename=paste(filename,".gri",sep="")
	}
	
	if(verbose) { print("Building header.") }
	outraster <- build_raster_header(x_filename=filename,reference_raster=reference_raster,
			out_nlayers=outbands,dataType='FLT8S',format=outformat)
	
	if(verbose) { print("calc_hpc complete.") }
	return(outraster)
}

