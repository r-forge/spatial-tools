#' @export

rasterToPolygons_hpc <- function(x, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE,
		tilesize = c(128,128),verbose=FALSE,tile_prefix,
		driver="GML",tile_suffix=".gml",layer="value")
{
	
	if(missing(tile_prefix))
	{
		tile_prefix=paste(tempfile(),"_",sep="")
	}
	x_extents <- raster_to_tile(x,tilesize=tilesize,crop=FALSE)
	
	rasterToPolygons_single <- function(id,x,x_extents,
		fun,n,na.rm,digits,dissolve,
		tile_prefix,max_number,
		driver,tile_suffix,layer)
	{
		raster_sub <- crop(x,x_extents[[id]])
		pol <- rasterToPolygons(x=raster_sub,fun,n,na.rm,digits,dissolve)
		outname <- paste(tile_prefix,add_leading_zeroes(number=id,max_number=max_number),
				tile_suffix,sep="")
#		save(pol,file=outname)
		writeOGR(pol,driver=driver,dsn=outname,layer=layer)
		return(outname)
	}
	
	rasterToPolygons_list <- foreach(id=seq(x_extents), 
		.packages=c("raster","rgdal","rgeos","spatial.tools"),.verbose=verbose) %dopar%
		rasterToPolygons_single(id,x,x_extents,fun,n,na.rm,digits,dissolve,
			tile_prefix,max_number=length(x_extents),driver,tile_suffix,layer)
	
	return(rasterToPolygons_list)
	
}