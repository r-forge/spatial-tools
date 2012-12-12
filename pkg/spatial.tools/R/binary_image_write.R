#' binary_image_write
#' Writes image data to a flat binary file using col/row/band positioning.
#' 
#' @param filename Character.  The path and filename of a "blank" binary file to store the image data.
#' @param mode The mode of data on disk.  Defaults to real64() (double precision floating point).
#' @param image_dims Vector. Vector of length(image_dims)==3 representing the number of columns, rows and bands in the output image.
#' @param interleave Character. The require output interleave.  By default is "BSQ". OTHER INTERLEAVES CURRENTLY UNSUPPORTED.
#' @param data Vector, matrix, array, or other data source which is coercible to a vector. This is the data to be written to the image.
#' @param data_position TBD
#' 
#' @return NULL
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[mmap]{mmap}}
#' @keywords mmap
#' @examples
#' # TODO
#' @export

binary_image_write=function(filename,mode=real64(),image_dims,interleave="BSQ",
	data,data_position)
{
	# data_position should be of format rbind(col_pos,row_pos,band_pos)
	# UL corner is 1,1,1
	
	if(class(data_position)=="list")
	{
		data_position=
			t(expand.grid(data_position[[1]],data_position[[2]],data_position[[3]]))
	}
	
	if(dim(data_position)[1]==2)
	{
		rbind(data_position,rep(1,dim(data_position)[2]))
	}
	
	# Some error checking up here.
	
	image_x=image_dims[1]
	image_y=image_dims[2]
	if(length(image_dims)==3)
	{
		image_z=image_dims[3]
	} else
	{
		image_z=1
	}
	
	if(interleave=="BSQ")
	{
		cell_position=as.integer(
			((data_position[2,]-1)*image_x)+
			(data_position[1,])+
			((data_position[3,]-1)*(image_x*image_y))
			)
	}
	
#	print(cell_position)
	
	if(class(data)=="array")
	{
		data=as.matrix(data,nrow=image_x*image_y,ncol=image_z)
	}
	
	out = mmap(filename, mode=mode)
	out[cell_position] <- as.numeric(data)
	munmap(out)	
}