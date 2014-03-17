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

predict <- function(object,...)
{
	if("newdata" %in% ls())
	{
		if(is.Raster(newdata))
		{
			predict.rasterEngine_function <- function(object,newdata,...)
			{
				# Determine all parameters that are not newdata and object:
				local_objects <- ls()
				model_parameters <- setdiff(local_objects,c("newdata","object"))
				
				#	library(plyr)
				# Receives the chunk as an array, needs to coerce it to data.frame
				# This can probably be a LOT more efficient.
				newdata_matrix <- aperm(newdata,c(3,2,1))
				dim(newdata_matrix) <- c(dim(newdata)[3],prod(dim(newdata)[1:2]))
				newdata_df <- as.data.frame(t(newdata_matrix))
				names(newdata_df) <- dimnames(newdata)[3][[1]]
				
				predict_output <- predict(object=object,newdata=newdata_df,mget(model_parameters))
				
				nbands_output <- length(predict_output)/prod(dim(newdata)[1:2])
				
				if(class(predict_output)=="factor")
				{
					predict_output <- as.numeric(predict_output)
				}
				
				predict_output_array <- array(predict_output,dim=c(dim(newdata)[1:2],nbands_output))
				
				return(predict_output_array)
			}
			
			output <- rasterEngine(newdata=newdata,fun=predict.rasterEngine_function,
					args=list(object=object,...))
			
			return(output)
		}
	}
	return(predict(object,...))
}