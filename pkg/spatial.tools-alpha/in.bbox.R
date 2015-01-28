

in.bbox <- function(x,bbox)
{
	# Coerce various formats to bbox matrix format:
	if(class(bbox)!="matrix" && !identical(colnames(bbox),c("min","max")))
	{
		# Convert here
	}
	
	if(class(x)!="matrix" && !identical(x,c("min","max")))
	{
		# Convert here
	}
	
	lx_in <- x[1,1] >= bbox[1,1] && x[1,1] <= bbox[1,2]
	rx_in <- x[1,2] >= bbox[1,1] && x[1,1] <= bbox[1,2]
	ly_in <- x[2,1] >= bbox[2,1] && x[2,1] <= bbox[2,2]
	uy_in <- x[2,2] >= bbox[2,1] && x[2,2] <= bbox[2,2]
	
	return(lx_in & rx_in & ly_in & uy_in)
	
}