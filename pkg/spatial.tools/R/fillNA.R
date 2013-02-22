##' Fill in NA regions from adjacent regions.
##' @title fillNA
##' @param x Raster*. Raster* object containing NA values to be filled in.
##' @param stopmask Raster*. (Optional) A Raster* mask with 0s at locations x should be filled in, and 1s for locations that should be left alone.
##' @param maxiter Numeric. The algorithm is iterative, so if you want to have it stop after a certain number of iterations, set the value here.  Defaults to 1000.
##' @param disable_cl logical. Disable parallel computing? Default is FALSE. 
##' @param verbose logical. Enable verbose execution? Default is FALSE.  
##' @param ... Other parameters to pass to 
##' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})

fillNA <- function(x,stopmask=NULL,maxiter=1000,verbose=FALSE,disable_cl=FALSE,...)
{
#	print(disable_cl)
	if(is.null(stopmask))
	{
		summary_x=summary(x,maxsamp=ncell(x))
	} else
	{
		stopmask_filled=stopmask
		stopmask_filled[stopmask==1]=x[stopmask==1]
		summary_x=summary(stopmask_filled,maxsamp=ncell(stopmask_filled))
		
		initial_mask=is.finite(x)
		initial_mask[stopmask==1]=1
		
#		remask=initial_mask
#		remask[stopmask==1]
	}
	
	summary_x_length=length(summary_x)
	NAs_idx=c(1:summary_x_length)[rownames(summary_x)=="NA's"]
	
	iter=1
	x_filled=x
	if(summary_x[NAs_idx]>0)
	{
		noNAs=FALSE
	} else
	{
		noNAs=TRUE
	}
	
	if(verbose){print(paste("Current NAs remaining to be filled:",summary_x[NAs_idx]))}
	
	fillNA_function=function(x,window_center,...)
	{
		# Only works for single layers right now
#		print(window_center)
		x_center=x[window_center[1],window_center[2],]
		if(is.na(x_center))
		{
			x_vector=as.vector(x)
			x_vector_mean=mean(x_vector,na.rm=TRUE)
			if(is.nan(x_vector_mean))
			{
				return(NA)
			} else
			{
				return(x_vector_mean)
			}
		} else
		{
			return(x_center)
		}
	}
		
		
	while(iter <= maxiter && noNAs!="TRUE")
	{
		if(verbose) { print(paste("Iteration #",iter)) }
		x_focal=focal_hpc(x=x_filled,window_dims=c(3,3),fun=fillNA_function,
			verbose=verbose,disable_cl=disable_cl)
		if(verbose) { print("Finishing focal...") }
#		x_focal
#		if(verbose) { print(summary(x_focal)) }
		x_NA_mask=is.na(x_filled)
		x_filled[x_NA_mask]=x_focal[x_NA_mask]
	
		if(is.null(stopmask))
		{
			summary_x=summary(x_filled,maxsamp=ncell(x_filled))
		} else
		{
			stopmask_filled[stopmask==1]=x_filled[stopmask==1]
			summary_x=summary(stopmask_filled,maxsamp=ncell(stopmask_filled))
		}
		
		if(verbose) { 
			print("Summary:...") 
			print(summary_x)
		}
		summary_x_length_iter=length(summary_x)
		
#		print(summary_x[NAs_idx])
		
		if(summary_x_length_iter == summary_x_length)
		{
			noNAs=FALSE
			if(verbose){print(paste("Current NAs remaining to be filled:",summary_x[NAs_idx]))}
		} else
		{
			noNAs=TRUE
		}
	
		iter = iter+1
	}
	
#	if(!is.null(stopmask))
#	{
#		x_filled[initial_mask==0]=NA
#	}

	return(x_filled)
}