

singlePart_to_multiPart <- function(x)
{
	if(class(x)=="SpatialPolygonsDataFrame" || class(x)=="SpatialLinesDataFrame")
	{
		df <- x@data
	}
	
	if(class(x)=="SpatialPolygonsDataFrame" || class(x) =="SpatialPolygons")
	{
		polygons <- x@polygons
		singlePolygon_N <- sapply(polygons,function(x) length(x@Polygons))
		singlePolygon_ID <- sapply(polygons,function(x) x@ID)
		
		singlePolygon <- unlist(sapply(polygons,function(x) x@Polygons))
		
		singlePolygons <- mapply(function(x,ID) 
				{
					Polygons(list(x),ID=ID)
				},
				x=singlePolygon,ID=seq(length(singlePolygon))
		)
		
		
		# Preserve the IDs
		# http://stackoverflow.com/questions/2894775/how-can-you-replicate-each-row-of-an-r-data-frame-and-specify-the-number-of-repl
		
	#	rep_ids <- rep(singlePolygon_ID,singlePolygon_N)
		if(!missing(df))
		{
		df.expanded <- df[rep(seq.int(1,nrow(df)), singlePolygon_N),,drop=FALSE]
		df.expanded$MPIDs <-  rep(singlePolygon_ID,singlePolygon_N)
		} else
		{
			df.expanded <- data.frame(MPIDs=rep(singlePolygon_ID,singlePolygon_N))
		}
		singlePart_SpatialPolygons <- SpatialPolygonsDataFrame(SpatialPolygons(
			singlePolygons,proj4string=CRS(proj4string(x))),data=df.expanded,match.ID=FALSE)
		return(singlePart_SpatialPolygons)
	}
	
}