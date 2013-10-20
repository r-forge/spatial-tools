
rasterEnvelope <- function(x)
{
	x_CRS <- projection(x,asText=FALSE)
	bbox_x <- bbox(x)
	corner_coordinates <- matrix(
			c(
					bbox_x[1,1],bbox_x[2,1],
					bbox_x[1,2],bbox_x[2,1],
					bbox_x[1,2],bbox_x[2,2],
					bbox_x[1,1],bbox_x[2,2],
					bbox_x[1,1],bbox_x[2,1]),
					ncol=2,byrow=TRUE)
	
	rasterPolygon <- Polygon(coords=corner_coordinates)
	rasterPolygons <- Polygons(list(rasterPolygon),ID=1)
	rasterEnvelope <- SpatialPolygons(list(rasterPolygons),proj4string=x_CRS)
			
	return(rasterEnvelope)
}