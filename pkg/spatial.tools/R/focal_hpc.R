#' Performs focal raster calculations using a snowfall cluster.
#' @title focal_hpc
#' @param x Raster*. A Raster* used as the input into the function.  Multiple inputs should be stacked together.
#' @param fun function. A focal function to be applied to the image. The input to the function is an array representing a local neighborhood to be processed, and the outputs of the function should be a single pixel vector (see Details).
#' @param window_dims Vector. TBD.
#' @param window_center Vector. TBD.
#' @param processing_unit Character. TBD.
#' @param args list. Arguments to pass to the function (see ?mapply).
#' @param filename character. Filename of the output raster.
#' @param outformat character. Outformat of the raster. Must be a format usable by hdr(). Default is 'raster'. CURRENTLY UNSUPPORTED.
#' @param overwrite logical. Allow files to be overwritten? Default is FALSE.
#' @param verbose logical. Enable verbose execution? Default is FALSE.  
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{clusterMap}}, \code{\link{mmap}}, \code{\link{dataType}}, \code{\link{hdr}} 
#' @details TODO. focal_hpc is designed to execute a function on a Raster* object using a snowfall cluster 
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
#'  tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
#'	ndvi_function <- function(x,...)
#'	{
#'		red_band <- inchunk[,,2]
#'		nir_band <- inchunk[,,3]
#'		ndvi <- (nir_band - red_band)/(nir_band + red_band)
#'		return(ndvi)
#'	}
#'  tahoe_ndvi <- focal_hpc(x=tahoe_highrez,fun=ndvi_function)
#' @export

# TODO args should include useful input parameters
focal_hpc <- function(x, fun, window_dims=c(1,1), window_center, args=NULL, filename='', 
		overwrite=FALSE,outformat='raster',verbose=FALSE, processing_unit="window",
		chunk_nrows=20, ...) {
	require("raster")
	require("foreach")
#	require("snowfall")
	require("rgdal")
	require("mmap")
	require("abind")
	
	if(verbose) { total_start_time = proc.time() }
	
	# Do some file checks up here.
	
#	if(verbose)
#	{
#		print(args)
#	}
#	parallel::detectCores()
#	stop_cl=FALSE
#	if(disable_cl)
#	{
#		if(verbose) { print("Cluster disabled.") }
#		nodes <- 1
#	} else
#	{
#		if(verbose) { print("Cluster enabled, loading packages.") }
#		if (is.null(cl)) {
#			stop_cl=TRUE
#			cl <- sfGetCluster()
#			sfLibrary("snowfall",character.only=TRUE)
#			sfLibrary("rgdal",character.only=TRUE)
#			sfLibrary("raster",character.only=TRUE)
#			sfLibrary("mmap",character.only=TRUE)
#			sfLibrary("spatial.tools",character.only=TRUE)
#		}
#		nodes <- sfNodes()
#		if(verbose) { print(paste("Executing on ",nodes," nodes.",sep="")) }
#	}
	
	if(!getDoParRegistered())
	{
		registerDoSEQ()
	}
	
	nodes <- getDoParWorkers()
	
	# Set up initial parameters
#	if(missing(window_dims))
#	{
#		print("Missing window_dims, please supply the window size as a vector.")
#		return()
#	}
	
	if(length(window_dims)==1)
	{
		window_dims=c(window_dims,window_dims)
	}
	
	if(missing(window_center))
	{
		if(verbose) { print("Missing window_center, assuming the center of the window...") }
		window_center=c(ceiling(window_dims[1]/2),ceiling(window_dims[2]/2))
	}
	
	if(verbose) { print(paste("window_dims:",window_dims,sep="")) }
	if(verbose) { print(paste("window_center:",window_center,sep="")) }
	
	window_rows=window_dims[2]
	window_cols=window_dims[1]
	
	layer_names=names(x)
#	print(layer_names)
	
	if(window_dims[1]>1 || window_dims[2]>1)
	{
		if(verbose) { print("Focal processing mode...") }
		processing_mode="focal"
		if(missing(processing_unit))
		{
			processing_unit="window"
		}
	} else
	{
		if(verbose) { print("Pixel processing mode...") }
		processing_mode="pixel"
		if(missing(processing_unit))
		{
			processing_unit="chunk"
		}
	}
	
	startrow_offset=-(window_center[2]-1)
	endrow_offset=window_rows-window_center[2]
	
	# Add additional info to the args.
	if(!is.null(args)) {
		args$window_center=window_center
		args$window_dims=window_dims
		args$layer_names=layer_names
	} else
	{
		args=list(window_center=window_center)
		args$window_dims=window_dims
		args$layer_names=layer_names
	}
	
	# We are going to pull out the first row and first two pixels to check the function...
	if(verbose) { print("Checking the function on a small chunk of data.") }
#	if(processing_mode=="focal")
#	{
#		pre_check=array(data=(1:(window_dims[1]*window_dims[2]*nlayers(x))),
#			dim=c(window_dims[1],window_dims[2],nlayers(x)))
#		r_check_args=args
#		r_check_args$x=pre_check
#		r_check_function <- do.call(fun, r_check_args)
#		
#	}
	
	if(processing_unit=="window")
	{
		if(verbose) { print("processing_unit=window...")}
		r_check <- getValuesBlock_enhanced(x, r1=1, r2=window_dims[2], c1=1,c2=window_dims[1])		
	} else
	{
		# The function works on the entire chunk.
		if(verbose) { print("processing_unit=chunk...")}
		r_check <- getValuesBlock_enhanced(x, r1=1, r2=window_dims[2], c1=1,c2=ncol(x))
	}
#	print(dimnames(r_check))
	
	# Add additional info to the args.
	r_check_args=args
	r_check_args$x=r_check
	r_check_function <- do.call(fun, r_check_args)
	
#	print(class(r_check_function))
#	print(dim(r_check_function)[3])
	
#	print(processing_unit)
#	print((r_check_function))	
	
	if(processing_unit=="window")
	{
		if(class(r_check_function)!="numeric")
		{
			print("window processing units require numeric vector outputs.  Please check your function.")
			return(NULL)
		} else
		{
			outbands=length(r_check_function)
		}
	}
	
	if(processing_unit=="chunk")
	{
#		print("Whee!")
		if(class(r_check_function)!="array" || 
				dim(r_check_function)[1] != ncol(x)||
				dim(r_check_function)[2] != window_dims[2])
		{
			print("chunk processing units require array vector outputs.  Please check your function.")
			print(dim(r_check_function))
			return(NULL)
		} else
		{
			outbands=dim(r_check_function)[3]
		}
	}
	
#	if(class(r_check_function)=="numeric" || class(r_check_function)=="integer" || 
#			(class(r_check_function)=="logical" && length(r_check_function)==1))
#	{
#		outbands=length(r_check_function)
#	} else
#	{
#		outbands=dim(r_check_function)[2]
#	}
	
#	outbands=dim(r_check_function)[1]
	
	if(verbose) { print(paste("Number of output bands determined to be:",outbands,sep=" ")) }
	
#	tr=blockSize(x,minrows=window_rows,n=nodes*2*outbands)
	tr=blockSize(x,chunksize=(chunk_nrows*nodes+(window_dims[2]-1))*ncol(x))
	
	if (tr$n < nodes) {
		nodes <- tr$n
	}
	tr$row2 <- tr$row + tr$nrows - 1
	
	tr$focal_row=tr$row+startrow_offset
	tr$focal_row2=tr$row2+endrow_offset
	
	tr$focal_row[tr$focal_row<1]=1
	tr$focal_row2[tr$focal_row2>nrow(x)]=nrow(x)
	
#	i=1:tr$n
	
	texture_tr=list(rowcenters=((tr$row[1]:tr$row2[1])-startrow_offset))
	texture_tr$row=texture_tr$rowcenters+startrow_offset
	texture_tr$row2=texture_tr$rowcenters+endrow_offset
	
	# We should test a single chunk here to see the size of the output...
	
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
	
	if(verbose) { print("Loading chunk arguments.") }
#	args=
	chunkArgs = list(fun=fun,x=x,x_ncol=ncol(x),tr=tr,
			window_dims=window_dims,window_center=window_center,
			layer_names=layer_names,
			args=args,filename=filename,
			outbands=outbands,processing_unit=processing_unit,
			verbose=verbose,layer_names=layer_names)
	
	if(verbose) { print("Loading chunk function.") }
	focalChunkFunction <- function(chunk,chunkArgs,...)
	{	
#		print(chunkArgs)
		e <- list2env(chunkArgs,envir=environment())
#		print(processing_unit)
	
		if(processing_unit=="window")
		{
			# If the function is designed to work on a single array...
			window_index=1:ncol(x)
			r_out=mapply(
					function(window_index,x,args,window_dims)
					{
						x_array=array(data=as.vector(x[(window_index:(window_index+window_dims[2]-1)),,]),
								dim=c(length((window_index:(window_index+window_dims[2]-1))),dim(x)[2],dim(x)[3])
						)
						dimnames(x_array)[[3]]=layer_names
#					if(is.null(args)) {
#						fun_args=list(x=x_array)
#						r_out <- do.call(fun, fun_args)
#					} else
#					{
						fun_args=args
						fun_args$x=x_array
						r_out <- do.call(fun, fun_args)
#					}	
#					return(as.numeric(r_out))
						return(r_out)
					},
					window_index,
					MoreArgs=list(x=chunk$processing_chunk,args=args,window_dims=window_dims)
			)
		} else
		{
			# The function works on the entire chunk.
#			if(is.null(args)) {
#				fun_args=list(x=chunk$processing_chunk)
#				r_out <- do.call(fun, fun_args)
#			} else
#			{
			fun_args=args
			fun_args$x=chunk$processing_chunk
			dimnames(fun_args$x)[[3]]=layer_names
#				print(fun_args)
			r_out <- do.call(fun, fun_args)
#			}	
		}
#		print(dim(r_out))
		image_dims=dim(x)
		image_dims=c(image_dims[2],image_dims[1],image_dims[3])
		chunk_position=list(
				1:ncol(x),
				chunk$row_center,
				1:outbands
		)
		
		parallel_write=function(filename,r_out,image_dims,chunk_position)
		{
			binary_image_write(filename=filename,mode=real64(),image_dims=image_dims,
					interleave="BSQ",data=r_out,data_position=chunk_position)
		}
#		if(!disable_cl)
#		{
		writeSuccess=FALSE
		while(!writeSuccess)
		{
			writeSuccess=TRUE
			tryCatch(parallel_write(filename,r_out,image_dims,chunk_position),
					error=function(err) writeSuccess <<- FALSE)	
		}
#		} else
#		{
#			parallel_write(filename,r_out,image_dims,chunk_position)
#		}
#		print("Here?")
	}
	
	# Begin the loop
	for(i in 1:tr$n)
	{
		if(verbose) { print(paste("Iteration: ",i," of ",tr$n,sep="")) }
		
		if(i==1)
		{
			r <- getValuesBlock_enhanced(x, r1=tr$focal_row[i], r2=tr$focal_row2[i], c1=1, c2=ncol(x))
		} else
		{
			r <- getValuesBlock_enhanced(x, r1=(tr$focal_row2[(i-1)]+1), r2=tr$focal_row2[i], c1=1, c2=ncol(x))
		}
		
		if(i==1)
		{
			# Add top cap
			if((1-(tr$row[1]+startrow_offset))>0)
			{
				r=abind(
						array(data=NA,dim=c(ncol(x),(1-(tr$row[1]+startrow_offset)),nlayers(x))),
						r,
						along=2)
			}
		}
		
		if(i==tr$n)
		{
			# Add bottom cap
			if(nrow(x)-tr$row2[tr$n]+endrow_offset>0)
				r=abind(r,
						array(data=NA,dim=c(ncol(x),(nrow(x)-tr$row2[tr$n]+endrow_offset),nlayers(x))),
						along=2)
		}
		
		# TODO: WE NEED TO BE ABLE TO SUBTRACT STUFF HERE ALSO (if center is outside)
		left_cap=window_center[2]-1
		right_cap=window_dims[2]-window_center[2]
		
		if(left_cap>0)
		{
			# Add left cap.
#			if(verbose) { print("Adding left cap...") }
			r=abind(
					array(data=NA,dim=c(left_cap,dim(r)[2],dim(r)[3])),
					r,
					along=1)
		}
		
		if(right_cap>0)
		{
			# Add right cap.
			r=abind(
					r,
					array(data=NA,dim=c(right_cap,dim(r)[2],dim(r)[3])),
					along=1)
		}
		
		if(i>1 && window_dims[2]>1)
		{
			r <- abind(r_old,r,along=2)
		}
		
		# We need to divide up the chunks here.
		# This is going to cause memory issues if we aren't careful...
		j=1:tr$nrows[i]
		row_centers=tr$row[i]:tr$row2[i]
		chunkList=mapply(function(j,r,texture_tr,row_centers)
				{				
					out_chunk=list(row_center=row_centers[j],
							processing_chunk=array(data=as.vector(r[,texture_tr$row[j]:texture_tr$row2[j],]),
									dim=c(dim(r)[1],length(texture_tr$row[j]:texture_tr$row2[j]),dim(r)[3]))
					)
					return(out_chunk)
				}
				,j,MoreArgs=list(r=r,texture_tr=texture_tr,row_centers=row_centers),
				SIMPLIFY=FALSE)
		
		# Parallel processing here.
#		if(disable_cl)
#		{
#			mapply(focalChunkFunction,chunkList,MoreArgs=chunkArgs)
#		} else
#		{
#			clusterMap(cl,focalChunkFunction,chunkList,MoreArgs=chunkArgs)
#		}
		
# Trying to use foreach instead:
		# Testing:
#		focalChunkFunction(chunkList=chunkList[[1]],chunkArgs=chunkArgs,...)

		foreach(chunk=chunkList, .packages=c("raster","rgdal","spatial.tools","mmap")) %dopar% 
				focalChunkFunction(chunk,chunkArgs,...)
		
		# Preserve the read data needed for the next iteration.
		if(i<tr$n && window_dims[2] > 1)
		{
			r_old=array(data=r[,(dim(r)[2]-(tr$focal_row2[i]-tr$focal_row[i+1])):dim(r)[2],],
					dim=c(dim(r)[1],length((dim(r)[2]-(tr$focal_row2[i]-tr$focal_row[i+1])):dim(r)[2]),dim(r)[3])
			)
		}
	}
	
	# Let's see if we can trick raster into making us a proper header
	if(verbose) { print("Setting up output header.") }
	if(outbands > 1) 
	{ 
		reference_raster=brick(raster(x,layer=1),nl=outbands) 
	} else
	{
		if(nlayers(x) > 1) { 
			reference_raster=raster(x,layer=1) 
		} else
		{ 
			reference_raster=x
		}	
	}
	
	if(outformat=='raster') { 
		if(verbose) { print("Outformat is raster.  Appending .gri to filename.") }
		file.rename(filename,paste(filename,".gri",sep=""))
		filename=paste(filename,".gri",sep="")
	}
	
	if(verbose) { print("Building header.") }
	outraster <- build_raster_header(x_filename=filename,
			reference_raster=reference_raster,
			out_nlayers=outbands,dataType='FLT8S',format=outformat,bandorder='BSQ')
	
	if(verbose) { print("focal_hpc complete.") }
	
	if(verbose) { print("Computation finished.  Timing:") }
	if(verbose) { print(proc.time()-total_start_time) }
	
	return(outraster)
}

