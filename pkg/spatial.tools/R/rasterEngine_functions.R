# Various pre-made rasterEngine functions

predict.rfsrc.rasterEngine <- function(newdata,object,probs,ncores,...)
{
	# For randomForestSRC, set rasterEngine to chunk_format="data.frame"
	
	local_objects <- ls()
	model_parameters <- setdiff(local_objects,c("newdata","object","probs","ncores"))
	
	# Parallel processing 
	
	options(rf.cores = ncores)
	options(mc.cores = 1)
	
	# This could be sped up by removing incomplete cases...
	newdata_complete <- complete.cases(newdata)
	newdata[is.na(newdata)] <- 1
	
	if(length(model_parameters)>0)
	{
		predict_output <- predict(object=object,newdata=newdata,mget(model_parameters))
	} else
	{
		# Fix for rfsrc quantileReg:
		if(!is.null(probs))
		{
			predict_output <- quantileReg(obj=object,prob=prob,newdata=newdata)
		} else
		{
			predict_output <- predict(object=object,newdata=newdata)
		}
	}
	
	# Get the correct columns...
	if(predict_output$family != "class")
	{
		# New fix for multivariate forests:
		if(predict_output$family == "regr+")
		{
			predict_output <- sapply(predict_output$regrOutput,function(x) return(x$predicted)) 
		}else
		{
			predict_output <- predict_output$predicted
		}
	} else
	{
		predict_output <- predict_output$class	
	}
	
	# Fill in nodata locations...
	if(!is.null(newdata_complete))
	{
		if(!is.null(dim(predict_output)))
		{
			if(length(dim(predict_output))>1)
			{
				predict_output[!newdata_complete,] <- NA
			} else
			{
				predict_output[!newdata_complete] <- NA
			}
		} else
		{
			predict_output[!newdata_complete] <- NA
		}
		
	}
	
	return(predict_output)
}