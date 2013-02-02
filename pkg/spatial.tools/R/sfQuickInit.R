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

sfQuickInit <- function(...)
{
	require("snowfall")
	numCores <- parallel::detectCores()
	sfInit(cpus=numCores,parallel=TRUE,...)
	if(any(search()=="package:foreach"))
	{
		require("doSNOW")
		cl <- sfGetCluster()
		registerDoSNOW(cl)
	}
}