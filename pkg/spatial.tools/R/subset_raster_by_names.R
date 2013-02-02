#' Subsets a raster based on its layer names
#' @title subset_raster_by_names
#' @param x A Raster* object to be subsetted.
#' @param subset_names Character. A vector of layer names to use to subset the Raster*.
#' @param allnames Logical. Make sure all subset_names are contained by x?
#' @name subset_raster_by_names
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @export


subset_raster_by_names=function(x,subset_names,allnames=TRUE)
{
	raster_names=names(x)
	
	if(allnames)
	{
		test_allnames=intersect(raster_names,subset_names)
		if(length(test_allnames)<length(subset_names))
		{
			print("Missing some layers in the input raster...")
			return(NULL)
		}
	}
	
	subset_index=sapply(raster_names,FUN=function(raster_names,subset_names)
			{ return(raster_names %in% subset_names) },subset_names=subset_names,simplify=TRUE )
	
	raw_data=getValues(boundary_analysis_brick)
	layer_index=1:nlayers(x)
	layer_index=layer_index[subset_index]
	x_out=x[[layer_index]]
	names(x_out)=raster_names[subset_index]
	return(x_out)
	
	
}