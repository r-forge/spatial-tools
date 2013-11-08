
# Brute force, somewhat.  Need to try to implement e.g.:
# http://stackoverflow.com/questions/240778/random-points-inside-a-polygon

spSample_stratified <- function(x,n,strata)
{

	
	# First, generate singlepart polys:
	x_singlePart <- singlePart_to_multiPart(x)
	
	# Now determine the area of each individual poly for weighting:
	x_singlePart_area <- sapply(x_singlePart@polygons,function(x) x@area)
	
	if(missing(strata))
	{
		strata <- "MPIDs"
	}
	
	
	x_singlePart_df <- x_singlePart@data
	
	x_strata_column <- x_singlePart_df[,strata]
	
	randomPoints <- foreach(current_strata=unique(x_strata_column),.packages=c("rgeos","sp"),.combine=rbind) %do%
			{
				spPolygons_strata_ids <- which(x_strata_column==current_strata)
				x_singlePart_area_strata <- x_singlePart_area[spPolygons_strata_ids]
				
				# Choose polygons by their areas
				spsample_area_weighted <- sample(spPolygons_strata_ids,n,replace=TRUE,prob=x_singlePart_area_strata)
				
				spPolygons_spsample <- spChFIDs(x_singlePart[spsample_area_weighted,],as.character(seq(n)))	
				
				# Generate 1 point per polygon:
				spPoints_strata_random <- foreach(nID=seq(n),.packages=c("sp","rgeos"),.combine=rbind) %dopar%
						{
						#	print(nID)
							# Coerce to gpc poly
							#spTogpc <- as(spPolygons_spsample[nID,],"gpc.poly")
							#spTogpc_triangulate <- triangulate(spTogpc)
							
							#coords <- spPolygons_spsample[nID,]@polygons[[1]]@Polygons[[1]]@coords
							#triangulated <- triangulate(coords)
							
							
							# Brute force approach:
						#	browser()
							envelope <- gEnvelope(spPolygons_spsample[nID,])
							
							pt_chosen = FALSE
							while(!pt_chosen)
							{
								
								spPoint_random <- spsample(envelope,n=1,type="random")
								spPoint_intersection <- gIntersection(spPoint_random,spPolygons_spsample[nID,])
							#	print(spPoint_intersection)
								if(!is.null(spPoint_intersection)) return(spPoint_random)
							}
						}
				
				
				return(SpatialPointsDataFrame(spPoints_strata_random,data.frame(strata=rep(current_strata,n))))
				
				
			}
	
	

	
	return(randomPoints)
	
}
#
#setwd("D:\\TEMP\\costell4\\Sampling")
#x <- readOGR(dsn=".",layer="clipped_union_v02")
#
#test1 <- spSample_stratified(x,n=50)