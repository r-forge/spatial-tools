
gIntersection_hpc <- function(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_not_poly, drop_lower_td=FALSE)
{
	# http://stackoverflow.com/questions/27558115/improve-speed-use-of-gdistance-function-by-using-parallel-processing-and-or-plyr

	# Determine the number of workers:
	nworkers <- getDoParWorkers()
	
	# Cribbed from parallel:
	splitIndices <- function (nx, ncl) 
	{
		i <- seq_len(nx)
		if (ncl == 0L) 
			list()
		else if (ncl == 1L || nx == 1L) 
			list(i)
		else {
			fuzz <- min((nx - 1L)/1000, 0.4 * nx/ncl)
			breaks <- seq(1 - fuzz, nx + fuzz, length = ncl + 1L)
			structure(split(i, cut(i, breaks)), names = NULL)
		}
	}
	
	length()
	
	ID.Split <- splitIndices(length(spgeom1),nworkers)
	
	intersected <- foreach(i=ID.Split,.packages="rgeos") %dopar%
			{
				return(
						gIntersection(spgeom1=spgeom1[i,],spgeom2=spgeom2,
								byid=byid, id=id, drop_lower_td=drop_lower_td)
				) 
			}
	# We need a slightly more complicated combine:
	intersected_merged <- spRbind_simple(spList=intersected)
	
}

if (require(maptools)) {
	xx <- readShapeSpatial(system.file("shapes/fylk-val-ll.shp", package="maptools")[1],
			proj4string=CRS("+proj=longlat +datum=WGS84"))
	bbxx <- bbox(xx)
	wdb_lines <- system.file("share/wdb_borders_c.b", package="maptools")
	xxx <- Rgshhs(wdb_lines, xlim=bbxx[1,], ylim=bbxx[2,])$SP
	res <-gIntersection(xx, xxx)
	plot(xx, axes=TRUE)
	plot(xxx, lty=2, add=TRUE)
	plot(res, add=TRUE, pch=16,col='red')
}
pol <- readWKT(paste("POLYGON((-180 -20, -140 55, 10 0, -140 -60, -180 -20),",
				"(-150 -20, -100 -10, -110 20, -150 -20))"))
library(sp)
GT <- GridTopology(c(-175, -85), c(10, 10), c(36, 18))
gr <- as(as(SpatialGrid(GT), "SpatialPixels"), "SpatialPolygons")
try(res <- gIntersection(pol, gr, byid=TRUE))
res <- gIntersection(pol, gr, byid=TRUE, drop_lower_td=TRUE)
# Robert Hijmans difficult intersection case
load(system.file("test_cases/polys.RData", package="rgeos"))
try(res <- gIntersection(a, b, byid=TRUE))
res <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
unlist(sapply(slot(res, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
oT <- get_RGEOS_polyThreshold()
oW <- get_RGEOS_warnSlivers()
oD <- get_RGEOS_dropSlivers()
set_RGEOS_polyThreshold(1e-3)
set_RGEOS_warnSlivers(TRUE)
res1 <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
unlist(sapply(slot(res1, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(TRUE)
res2 <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
unlist(sapply(slot(res2, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(FALSE)
oo <- gUnaryUnion(res1, c(rep("1", 3), "2", "3", "4"))
unlist(sapply(slot(oo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
ooo <- gIntersection(b, oo, byid=TRUE)
gArea(ooo, byid=TRUE)
unlist(sapply(slot(ooo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(TRUE)
ooo <- gIntersection(b, oo, byid=TRUE)
gArea(ooo, byid=TRUE)
unlist(sapply(slot(ooo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_polyThreshold(oT)
set_RGEOS_warnSlivers(oW)
set_RGEOS_dropSlivers(oD)
