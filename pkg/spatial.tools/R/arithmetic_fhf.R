


arith_hpc <- function(x,y)
{
	# Check for assumptions
	nlayers_x <- nlayers(x)
	nlayers_y <- nlayers(y)
	input <- stack(x,y)
	
	add_fhf <- function(x,nlayers_x,...)
	{
#	print(dim(x))
		x_sub <- x[,,(1:nlayers_x),drop=FALSE]
		y_sub <- x[,,((nlayers_x+1):dim(x)[3]),drop=FALSE]
		out_dim <- c(dim(x)[1],dim(x)[2],max(dim(x_sub)[3],dim(y_sub)[3]))
#		print(out_dim)
		xy_add <- array(as.numeric(x_sub)+as.numeric(y_sub),dim=out_dim)
		return(xy_add)
	}
	
	output <- focal_hpc(input,add_fhf,args=list(nlayers_x),verbose=TRUE)
	return(output)
	
}
	