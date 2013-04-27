pkgname <- "spatial.tools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('spatial.tools')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_leading_zeroes")
### * add_leading_zeroes

flush(stderr()); flush(stdout())

### Name: add_leading_zeroes
### Title: Add Leading Zeroes to a Numeric Vector
### Aliases: add_leading_zeroes
### Keywords: format

### ** Examples

x=c(1:10)
add_leading_zeroes(x,number_length=4)
add_leading_zeroes(x,max_number=10000)



cleanEx()
nameEx("binary_image_write")
### * binary_image_write

flush(stderr()); flush(stdout())

### Name: binary_image_write
### Title: Writes image data to a flat binary file using col/row/band
###   positioning.
### Aliases: binary_image_write
### Keywords: mmap

### ** Examples

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
# Create a blank file using create_blank_raster
test_blank_file <- create_blank_raster(reference_raster=tahoe_highrez)
blank_raster <- brick(test_blank_file)
# It should be all 0s:
setMinMax(blank_raster)
# Write some ones to to the 100th line, columns 25 to 50, bands 1 and 3:
data_position <- list(25:50,100,c(1,3))
data1s <- array(1,dim=c(length(data_position[[1]]),length(data_position[[2]]),length(data_position[[3]])))
plot(raster(test_blank_file,layer=1))
binary_image_write(filename=test_blank_file,
	mode=real64(),image_dims=dim(tahoe_highrez),interleave="BSQ",
	data=data1s,data_position=data_position)
setMinMax(blank_raster)
plot(raster(blank_raster,layer=1))



cleanEx()
nameEx("build_raster_header")
### * build_raster_header

flush(stderr()); flush(stdout())

### Name: build_raster_header
### Title: Builds a raster header for a flat binary file.
### Aliases: build_raster_header

### ** Examples

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
test_blank_file <- create_blank_raster(filename=paste(tempfile(),".gri",sep=""),
	reference_raster=tahoe_highrez,nlayers=2,
	create_header=FALSE,format="raster",dataType="FLT8S",bandorder="BSQ")
test_blank_raster <- build_raster_header(x_filename=test_blank_file,
	reference_raster=tahoe_highrez,out_nlayers=2,
	dataType='FLT8S',format='raster',bandorder="BSQ",setMinMax=TRUE)
test_blank_raster



cleanEx()
nameEx("create_blank_raster")
### * create_blank_raster

flush(stderr()); flush(stdout())

### Name: create_blank_raster
### Title: Create an empty raster and header.
### Aliases: create_blank_raster

### ** Examples

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
test_blank_file <- create_blank_raster(reference_raster=tahoe_highrez)
file.info(test_blank_file)
test_blank_raster <- create_blank_raster(reference_raster=tahoe_highrez,return_filename=FALSE)
test_blank_raster



cleanEx()
nameEx("focal_hpc")
### * focal_hpc

flush(stderr()); flush(stdout())

### Name: focal_hpc
### Title: Engine for performing fast, easy to develop pixel and focal
###   raster calculations with parallel processing capability.
### Aliases: focal_hpc

### ** Examples

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
# Pixel-based processing:
	ndvi_function <- function(x,...)
	{
		# Note that x is received by the function as a 3-d array:
		red_band <- x[,,2]
		nir_band <- x[,,3]
		ndvi <- (nir_band - red_band)/(nir_band + red_band)
		# The output of the function should also be a 3-d array,
		# even if it is a single band:
		ndvi <- array(ndvi,dim=c(dim(x)[1],dim(x)[2],1))
		return(ndvi)
	}

	sfQuickInit(cpus=2)
 tahoe_ndvi <- focal_hpc(x=tahoe_highrez,fun=ndvi_function)
	sfQuickStop()

## Not run: 
##D # Focal-based processing:
##D local_smoother <- function(x,...)
##D {
##D  # Assumes a 3-d array representing
##D 	# a single local window, and return
##D  # a single value or a vector of values.
##D 	smoothed <- apply(x,3,mean)
##D 	return(smoothed)
##D }
##D # Apply the function to a 3x3 window:
##D sfQuickInit(cpus=2)
##D tahoe_3x3_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(3,3))
##D sfQuickStop()
##D 
##D # Example with 7 x 7 window in full parallel mode:
##D sfQuickInit()
##D tahoe_7x7_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(7,7))
##D sfQuickStop()
## End(Not run)



cleanEx()
nameEx("getValuesBlock_enhanced")
### * getValuesBlock_enhanced

flush(stderr()); flush(stdout())

### Name: getValuesBlock_enhanced
### Title: Easier-to-use function for grabbing a block of data out of a
###   Raster*.
### Aliases: getValuesBlock_enhanced

### ** Examples

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
mychunk <- getValuesBlock_enhanced(tahoe_highrez,r1=100,r2=110,c1=20,c2=50)
class(mychunk)
dim(mychunk)
mychunk_raster <- getValuesBlock_enhanced(tahoe_highrez,r1=100,r2=110,c1=20,c2=50,format="raster")
mychunk_raster



cleanEx()
nameEx("remove_file_extension")
### * remove_file_extension

flush(stderr()); flush(stdout())

### Name: remove_file_extension
### Title: remove_file_extension
### Aliases: remove_file_extension

### ** Examples

myfilename="my.file.gri"
remove_file_extension(myfilename,".")
remove_file_extension(myfilename,".file.gri")



cleanEx()
nameEx("sfQuickInit")
### * sfQuickInit

flush(stderr()); flush(stdout())

### Name: sfQuickInit
### Title: Quickly initializes a parallel snowfall cluster and registers it
###   with foreach.
### Aliases: sfQuickInit

### ** Examples

sfQuickInit(cpus=2)
sfQuickStop()



cleanEx()
nameEx("sfQuickStop")
### * sfQuickStop

flush(stderr()); flush(stdout())

### Name: sfQuickStop
### Title: Quickly stops a parallel snowfall cluster and deregisters it
###   from foreach.
### Aliases: sfQuickStop

### ** Examples

sfQuickInit(cpus=2)
sfQuickStop()



cleanEx()
nameEx("spatial_sync_vector")
### * spatial_sync_vector

flush(stderr()); flush(stdout())

### Name: spatial_sync_vector
### Title: Matches a vector's projection to another vector or raster
###   object's projection.
### Aliases: spatial_sync_vector

### ** Examples

tahoe_highrez_training_points_utm <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training_points_utm")
print(projection(tahoe_highrez_training_points_utm))
tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools"))
print(projection(tahoe_lidar_bareearth))
tahoe_highrez_training_points_utm_synced <- spatial_sync_vector(tahoe_highrez_training_points_utm,tahoe_lidar_bareearth)
print(projection(tahoe_highrez_training_points_utm_synced))



cleanEx()
nameEx("tahoe_highrez.tif")
### * tahoe_highrez.tif

flush(stderr()); flush(stdout())

### Name: tahoe_highrez.tif
### Title: High resolution false color infrared image from the Lake Tahoe
###   Basin.
### Aliases: tahoe_highrez.tif
### Keywords: data datasets

### ** Examples

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
plotRGB(tahoe_highrez)



cleanEx()
nameEx("tahoe_highrez_training")
### * tahoe_highrez_training

flush(stderr()); flush(stdout())

### Name: tahoe_highrez_training
### Title: Point and polygon files for use with spatial.tools
### Aliases: tahoe_highrez_training
### Keywords: data datasets

### ** Examples

tahoe_highrez_training_polygons <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training")
spplot(tahoe_highrez_training_polygons,zcol="Class")
tahoe_highrez_training_points <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training_points")
spplot(tahoe_highrez_training_points,zcol="SPECIES")
tahoe_highrez_training_points_utm <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training_points_utm")
print(projection(tahoe_highrez_training_points_utm))



cleanEx()
nameEx("tahoe_lidar_bareearth.tif")
### * tahoe_lidar_bareearth.tif

flush(stderr()); flush(stdout())

### Name: tahoe_lidar_bareearth.tif
### Title: Lidar-derived bare earth digital elevation model from the Lake
###   Tahoe Basin.
### Aliases: tahoe_lidar_bareearth.tif
### Keywords: data datasets

### ** Examples

tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools"))
plot(tahoe_lidar_bareearth)



cleanEx()
nameEx("tahoe_lidar_highesthit.tif")
### * tahoe_lidar_highesthit.tif

flush(stderr()); flush(stdout())

### Name: tahoe_lidar_highesthit.tif
### Title: Lidar-derived highest hit (aka canopy) digital elevation model
###   from the Lake Tahoe Basin.
### Aliases: tahoe_lidar_highesthit.tif
### Keywords: data datasets

### ** Examples

tahoe_lidar_highesthit <- raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools"))
plot(tahoe_lidar_highesthit)



cleanEx()
nameEx("which.max.simple")
### * which.max.simple

flush(stderr()); flush(stdout())

### Name: which.max.simple
### Title: Location of Maximum Value
### Aliases: which.max.simple
### Keywords: calculate

### ** Examples

## Not run: 
##D 
##D x<-c(2:4,1,1,NA)
##D y<-c(4,1:3,NA,4)
##D ## The index is only calculated for a unique maximum
##D which.max.simple(x)
##D which.max.simple(y)
##D which.max.simple(y,na.rm=FALSE)
##D which.max.simple(x,na.rm=FALSE)
## End(Not run)



cleanEx()
nameEx("which.min.simple")
### * which.min.simple

flush(stderr()); flush(stdout())

### Name: which.min.simple
### Title: Location of Minimum Value
### Aliases: which.min.simple
### Keywords: calculate

### ** Examples

## Not run: 
##D 
##D x<-c(4,1:3,NA,4)
##D y<-c(2:4,1,1,NA)
##D ## The index is only calculated for a unique minimum
##D which.min.simple(x)
##D which.min.simple(y)
##D which.min.simple(y,na.rm=FALSE)
##D which.min.simple(x,na.rm=FALSE)
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
