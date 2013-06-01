fillNA_fhf=function(x,window_center,...)
{
	# Only works for single layers right now
#	print(window_center)
	x_center=x[window_center[1],window_center[2],]
	if(is.na(x_center))
	{
		x_vector=as.vector(x)
		x_vector_mean=mean(x_vector,na.rm=TRUE)
		if(is.nan(x_vector_mean))
		{
			# print("NA")
			return(as.numeric(NA))
		} else
		{
			# print("xvecmean")
			return(x_vector_mean)
		}
	} else
	{
		# print("xcen")
		return(x_center)
	}
}