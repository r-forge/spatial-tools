#' getValuesBlock_enhanced
#' Easier-to-use function for grabbing data out of a Raster*.
#' 
#' @param x TODO.
#' @param r1 TODO.
#' @param r2 TODO.
#' @param c1 TODO.
#' @param c2 TODO.
#' @param format TODO.
#' @param ... TODO.
#' 
#' @return An array.
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[raster]{getValues}}
#' @examples
#' # TODO
#' @export

getValuesBlock_enhanced=function(x,r1,r2,c1,c2,format="array",...)
{
	# getValues(crop(x, extent(x, r1=1, r2=window_rows, c1=1,c2=window_cols)),format="matrix")
	
	layer_names=names(x)
	
	getvalues_raw=as.numeric(getValues(crop(x, extent(x, r1=r1, r2=r2, c1=c1,c2=c2))))
	getvalues_raw_nrows=r2-r1+1
	getvalues_raw_ncols=c2-c1+1
	getvalues_raw_nlayers=nlayers(x)
	
	# Test the input file.
	if(getvalues_raw_nlayers==1)
	{
	# Raster
		getvalues_array=array(data=getvalues_raw,
			dim=c(getvalues_raw_ncols,getvalues_raw_nrows,getvalues_raw_nlayers))		
	} else
	{
	# Brick or stack
		getvalues_array=array(data=getvalues_raw,
			dim=c(getvalues_raw_ncols,getvalues_raw_nrows,getvalues_raw_nlayers))
	}
	dimnames(getvalues_array)[[3]]=layer_names
	return(getvalues_array)
}