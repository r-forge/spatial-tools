
rasterMin <- function(x,...)
{
	minmatrix <- apply(x,1:2,min)
	return(array(minmatrix,dim=c(dim(x)[1],dim(x)[2],1)))	
}

rasterMax <- function(x,...)
{
	maxmatrix <- apply(x,1:2,max)
	return(array(maxmatrix,dim=c(dim(x)[1],dim(x)[2],1)))	
}