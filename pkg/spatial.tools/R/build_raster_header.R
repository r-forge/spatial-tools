#' Builds a raster header for a flat binary file.
#' @title build_raster_header
#' @param x_filename Character. The filename of the input binary file.
#' @param reference_raster Raster*. A Raster* object containing the header information to be used.
#' @param out_nlayers Numeric. The number of layers in the flat binary file (defaults to nlayers(reference_raster)).
#' @param dataType Character. The dataType of the flat binary file.  See ?dataType for available datatypes.  Default is 'FLT8S'.
#' @param format Character. The format of the header.  See ?hdr for valid entries.  Default is 'raster'.
#' @author Jonathan A. Greenberg and Robert Hijimans (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{dataType}} \code{\link{hdr}}
#' @examples
#' # TODO 
#' @export

build_raster_header <- function(x_filename,reference_raster,out_nlayers,
		dataType='FLT8S',format='raster')
{
	require("raster")
	if(missing(out_nlayers))
	{
		out_nlayers=nlayers(reference_raster)
	}
	if(out_nlayers==1)
	{
		outraster <- raster(reference_raster)
	} else
	{
		outraster <- brick(raster(reference_raster),nl=out_nlayers)
	}
	
	outraster@file@name <- x_filename
	outraster@file@datanotation <- dataType
	try(outhdr <- hdr(outraster, format='raster'),silent=TRUE)
	outraster=brick(paste(remove_file_extension(x_filename,".gri"),".grd",sep=""))
	return(outraster)
}