#' @export

raster_to_tile <- function(x,tilesize=c(1024,1024),crop=FALSE)
{
	ncol_x <- ncol(x)
	nrow_x <- nrow(x)
	
	xtiles_N <- ceiling(ncol_x/tilesize[1])
	ytiles_N <- ceiling(nrow_x/tilesize[2])
	
	tileList <- vector(mode="list",length=xtiles_N*ytiles_N)
	
	tileID <- 1
	for(i in seq(xtiles_N))
	{
		for(j in seq(ytiles_N))
		{
			tempTileLeft <- (i-1)*tilesize[1] + 1
			tempTileRight <- min((tempTileLeft+tilesize[1]-1),ncol_x)
			tempTileTop <- (j-1)*tilesize[2] + 1
			tempTileBottom <- min((tempTileTop+tilesize[2]-1),nrow_x)
			tempExtent <- extent(x,tempTileLeft,tempTileRight,tempTileTop,tempTileBottom)
			if(crop)
			{
				tileList[[tileID]] <- crop(x,tempExtent)	
			} else
			{
				tileList[[tileID]] <- tempExtent
			}
			tileID <- tileID+1
		}
	}
	return(tileList)
}