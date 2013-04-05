bbox_to_SpatialPolygons <- function(x,proj4string=CRS(as.character(NA)))
{	
	if(class(x)=="RasterLayer" || class(x)=="RasterBrick" || class(x)=="RasterStack")
	{
		bbox <- bbox(x)
		proj4string <- CRS(projection(x))
	}
	
	if(class(x)=="Extent")
	{
		bbox <- as.matrix(x,nrow=2,ncol=2)
	}
	
	if(class(x)=="matrix")
	{
		bbox <- x
	}
	
	coords <- rbind(
			c(bbox[1,1],bbox[2,1]),
			c(bbox[1,2],bbox[2,1]),
			c(bbox[1,2],bbox[2,2]),
			c(bbox[1,1],bbox[2,2]),
			c(bbox[1,1],bbox[2,1])
	)
	
	bboxPolygon <- Polygon(coords)
	bboxPolygons <- Polygons(list(bboxPolygon),ID=1)
	bboxSpatialPolygons <- SpatialPolygons(list(bboxPolygons),proj4string=proj4string)
	return(bboxSpatialPolygons)
}