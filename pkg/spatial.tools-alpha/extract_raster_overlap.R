
extract_raster_overlap <- function(x,y,outformat="SpatialPolygons",fix_extent=TRUE)
{
	if(class(x)!="list")
	{
		x <- list(x,y)	
	}
		
	# Determine bounding boxes for each 
	x_list_bbox <- sapply(x,bbox_to_SpatialPolygons,simplify=FALSE)
	
	# Convert 
	overlap_sp <- x_list_bbox[[1]]
	for(i in 2:length(x_list_bbox))
	{
		overlap_sp <- gIntersection(overlap_sp,x_list_bbox[[i]])
	}
	
	if(outformat=="SpatialPolygons")
	{
		return(overlap_sp)
	} 
	
	if(outformat=="Extent")
	{
		return(extent(bbox(overlap_sp)))	
	}
	
	if(outformat=="RasterList")
	{
		overlap_extent <- extent(bbox(overlap_sp))
		xcrop <- crop(x[[1]],overlap_extent)
		ycrop <- crop(x[[2]],overlap_extent)
		if(fix_extent) extent(ycrop) <- extent(xcrop)
		return(list(xcrop,ycrop))
	}
	
	if(outformat=="RasterStack")
	{
		overlap_extent <- extent(bbox(overlap_sp))
		xcrop <- crop(x[[1]],overlap_extent)
		ycrop <- crop(x[[2]],overlap_extent)
		if(fix_extent) extent(ycrop) <- extent(xcrop)
		return(stack(list(xcrop,ycrop)))
	}
	
}