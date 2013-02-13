#' sfQuickInit
#' 
#' Quickly initializes a parallel snowfall cluster.
#' 
#' @param ... parameters to pass to sfInit()
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[snowfall]{sfInit}}
#' @details (Even more) quickly start a snow cluster with maximum available
#' cpus, parallel = TRUE, and type = "SOCK" and registers it with foreach.  
#' @examples \dontrun{
#' sfQuickInit()
#' sfGetCluster()
#' sfStop()
#' }
#' @export

sfQuickInit <- function(cpus,...)
{
	require("snowfall")
#	require("snow")
	if(missing("cpus"))
	{
		cpus <- parallel::detectCores()
	}
	#	cl <- makeCluster(numCores,type="SOCK")	
	sfInit(cpus=cpus,parallel=TRUE,...)
	if(any(search()=="package:foreach"))
	{
		require("doSNOW")
		cl <- sfGetCluster()
		registerDoSNOW(cl)
	}
	return(cl)
}


