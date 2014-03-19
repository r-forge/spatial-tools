#' Model predictions (including Raster* objects)
#' @param object a model object for which prediction is desired.
#' @param ... additional arguments affecting the predictions produced.
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{predict}}
#' @details predict will operate normally, unless a parameter named "newdata"
#' is found and it is of class Raster*.  If this occurs, predict will use
#' rasterEngine to perform a prediction.  Currently, this works for predict.* 
#' statements in which the data to predict on is called by the parameter "newdata", 
#' the input data is in the form of a data.frame, and the output is a vector 
#' or matrix of numbers or factors.
#' 
#' predict will run in parallel if a cluster is registered
#' with foreach via a do* statement, or if the user uses sfQuickInit().
#'  
#' @examples
#' # TODO
#' @export

predict_rasterEngine <- function(object,...)
{
	list2env(list(...),envir=environment())
	if("newdata" %in% ls())
	{
		newdata <- newdata
		if(is.Raster(newdata))
		{
			predict.rasterEngine_function <- function(newdata,object,...)
			{
				# Determine all parameters that are not newdata and object:
				local_objects <- ls()
				model_parameters <- setdiff(local_objects,c("newdata","object"))
				
				newdata_dim <- dim(newdata)
				
				predictor_names <- dimnames(newdata)[3][[1]]
				
				newdata <- aperm(newdata,c(3,1,2))
				dim(newdata) <- c(newdata_dim[3],prod(newdata_dim[1:2]))
				newdata <- t(newdata)
				
				newdata_df <- as.data.frame(newdata)
				names(newdata_df) <- predictor_names
				
				predict_output <- predict(object=object,newdata=newdata_df,mget(model_parameters))
				
				nbands_output <- length(predict_output)/prod(newdata_dim[1:2])
				
				if(class(predict_output)=="factor")
				{
					predict_output <- as.numeric(predict_output)
				}
				
				predict_output_array <- array(predict_output,dim=c(newdata_dim[1:2],nbands_output))
				
				return(predict_output_array)
			}
			
			additional_args <- list(...)
			additional_args$newdata <- NULL
			additional_args <- c(list(object=object),unlist(additional_args,recursive=FALSE))
			
			output <- rasterEngine(newdata=newdata,fun=predict.rasterEngine_function,
					args=additional_args,.packages=(.packages()))
			
			return(output)
		}
	}
	return(predict(object,...))
}