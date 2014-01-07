# TODO: Check for negative bbox boundaries?

rasterToMesh <- function(raster)
{
	rasterCRS <- projection(raster,asText=FALSE)
	
	rasterbbox <- bbox(raster)
	
	rasterresx <- res(raster)[1]
	rasterresy <- res(raster)[2]
	
	rasterULx <- rasterbbox[1,1]
	rasterULy <- rasterbbox[2,1]
	
	rasterLx <- rasterULx+(seq(ncol(raster))-1)*rasterresx
	rasterRx <- rasterLx + rasterresx
	
	rasterUy <- rasterULy+(seq(nrow(raster))-1)*rasterresy
	rasterLy <- rasterUy + rasterresy
	
	system.time(
	rasterMesh <- foreach(rowID=seq(nrow(raster)),.packages=c("raster"),.combine=rbind) %dopar%
			{
				x_coords <- rbind(rasterLx,rasterRx,rasterRx,rasterLx,rasterLx)
				y_coords <- replicate(ncol(raster),c(rasterUy[rowID],rasterUy[rowID],rasterLy[rowID],rasterLy[rowID],rasterUy[rowID]))
				cellnumbers <- seq(ncol(raster)) + (rowID-1)*ncol(raster)
				
				polysList <- lapply(seq(ncol(raster)),function(x,x_coords,y_coords,cellnumbers)
						{
							coords <- cbind(x_coords[,x],y_coords[,x])
							poly <- Polygon(coords)
							polys <- Polygons(list(poly),ID=cellnumbers[x])
							return(polys)
						},x_coords=x_coords,y_coords=y_coords,cellnumbers=cellnumbers)
				
				spPolysSub <- SpatialPolygons(polysList,proj4string=rasterCRS)
				return(spPolysSub)
			}
)
	return(rasterMesh)
	
}

tahoe_lidar_highesthit <- raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools"))

raster <- tahoe_lidar_highesthit
class(projection(raster))