# rasterEngine Tutorial
# Jonathan A. Greenberg
# jgreenberg@unr.edu

# Last updated: 17 January 2018

##################################################################################################
########## Overview ##########
# I. Background
# II. Getting rasterEngine
# III. Pixel-based processing
# A. Single input raster, 1 band
# B. Single input raster, multiple bands
# C. Multiple input rasters
# D. Multiple output rasters
# E. Using non-raster inputs
# F. Using predict() with rasterEngine
# IV. Focal window processing
# A. Single input raster, 1 band
# B. Single input raster, multiple bands
# V. Debugging rasterEngine functions

##################################################################################################
########## I. Background ##########
# Background: rasterEngine provides a flexible framework
# for developing and deploying raster functions in a parallel
# environment. Because rasterEngine uses �foreach�, a large
# number of possible parallel backends can be used, including:
# -parallel (built into R)
# -multicore (built into R)
# -snow
# -MPI (via Rmpi)

# rasterEngine provides both pixel-based and focal-based (local
# neighborhood) processing. rasterEngine will read, process,
# and write chunks of the output in parallel.

# The general process for using rasterEngine is:
# 1) Import Raster*�s of the input data using the �raster� package.
# 2) Start a parallel backend using functions provided by, e.g.,
# the �parallel� or �Rmpi� packages.
# 3) Register the parallel backed with the foreach engine using
# a registerDo*() function (e.g. registerDoParallel())
# 4) Develop a function to be applied to a chunk of pixel data
# or a single local window.
# 5) Run rasterEngine:
# output <- rasterEngine(input=inputRaster,fun=chunkFunction,
# window_dims=c(window_x,window_y))
# 6) (Once finished) Shut down parallel backend.

# Acknowledgements: special thanks to Robert Hijimans (�raster�)
# Jeffrey Ryan (�mmap�) and Steve Weston (�foreach�).

########## II. Getting rasterEngine ##########
# rasterEngine is part of the spatial.tools package, which is
# available on CRAN. However, this tutorial expects version 1.1.1 or later
# of spatial.tools. If it is not available on CRAN:
install.packages("spatial.tools")
# The user should get the latest version from R-forge:
install.packages("spatial.tools", repos="http://R-Forge.R-project.org",type="source")
# Or the bleeding edge, using svn via the remotes package:
library("remotes")
remotes:::install_svn("svn://r-forge.r-project.org/svnroot/spatial-tools/pkg/spatial.tools")

##################################################################################################
########## III. Pixel-based processing ##########
# When a function is to be applied one pixel at a time (vs. a local neighborhood),
# rasterEngine provides the input data in an array or data.frame format representing a chunk
# of the input images. The reason for this is that R provides many vectorized operations,
# so passing one pixel at a time to the function often makes the function MUCH slower
# to execute. It is possible to use rasterEngine to deploy a pixel-at-a-time function,
# but the user should use an apply() statement to accomplish this.

##################################################################################################
##### A. Single input raster (1-band) #####

# Required packages. doParallel can be replaced with other
# foreach backends, e.g. doMPI, doSNOW, etc.
# Reference: http://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf

require("spatial.tools")
require("raster")

# Begin a parallel cluster and register it with foreach:
cpus = 12 # The number of nodes/cores to use in the cluster
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Note: you can use a conveinence script to do the same thing:
?sfQuickInit

# Load up a 1-band image:
tahoe_lidar_highesthit <-
		setMinMax(raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools")))
tahoe_lidar_highesthit
plot(tahoe_lidar_highesthit)

# Our function will convert the data from meters to feet above sea level:

meters_to_feet <- function(elevation_in_meters)
{
# elevation_in_meters is received by this function as an array
	feet <- elevation_in_meters * 3.28084
	return(feet)
}

# Test the function on an array:
test_input <- array(1:10,dim=c(5,2,1))
test_input
meters_to_feet(elevation_in_meters=test_input)

# Apply it to the input image using rasterEngine:
# Hint: run this twice to see the first-time run
# vs. subsequent runs. The first time the parallel
# engine is invoked, it has to load the packages into the workers.
# The second time, the packages are already loaded
# so the execution is faster.
system.time(
		tahoe_lidar_highesthit_feet <-
				rasterEngine(
# Match the variable name in the function to the raster:
						elevation_in_meters=tahoe_lidar_highesthit,
# Assign the function:
						fun=meters_to_feet
				)
)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

# Alternative function outputs:
meters_to_feet <- function(elevation_in_meters)
{
# elevation_in_meters is received by this function as an array
	feet <- elevation_in_meters * 3.28084
	return(feet)
}

system.time(
		tahoe_lidar_highesthit_feet <-
				rasterEngine(
# Match the variable name in the function to the raster:
						elevation_in_meters=tahoe_lidar_highesthit,
# Assign the function:
						fun=meters_to_feet,
# Send the chunk as a data.frame instead of the array:
						chunk_format="data.frame"
				)
)

##################################################################################################
##### B. Single input raster (Multiple bands) #####
# Because rasterEngine passes the chunk as an array
# where the third dimension represents the bands, we
# can easily write functions that take advantage of this.

require("spatial.tools")

# Begin a parallel cluster and register it with foreach:
cpus = 12 # The number of nodes/cores to use in the cluster
# This can be modified to fit a specific cluster/multicore computer setup
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Load up a 3-band image:
tahoe_highrez <- setMinMax(
		brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))
tahoe_highrez
plotRGB(tahoe_highrez)

# This function will calculate NDVI from the input raster. In this
# image, the red band is band #2, and the near-infrared band is band
# #3.

ndvi <- function(GRNIR_image)
{
# The input array will have dim(GRNIR_image)[3] equal
# to 3, because the input image had three bands.
# Note: the following two lines return a *matrix*, not
# an array:
	red_band <- GRNIR_image[,,2]
	nir_band <- GRNIR_image[,,3]
	
	ndvi <- (nir_band-red_band)/(nir_band + red_band)
	
# ndvi is a matrix, but we must return the value as
# an array. We will preserve the number of columns
# and rows of the input array, but set the output
# number of bands to be 1:
	
	dim(ndvi) <- c(
			dim(GRNIR_image)[1],
			dim(GRNIR_image)[2],
			1)
	return(ndvi)
}

system.time(
		tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi)
)

# We can skip the dim(ndvi) step by setting drop=FALSE:
ndvi_nodrop <- function(GRNIR_image)
{
# The input array will have dim(GRNIR_image)[3] equal
# to 3, because the input image had three bands.
# Note: the following two lines now return an array,
# so we don�t need to manually set the dim(ndvi) at the
# end:
	red_band <- GRNIR_image[,,2,drop=FALSE]
	nir_band <- GRNIR_image[,,3,drop=FALSE]
	
	ndvi <- (nir_band-red_band)/(nir_band + red_band)
	return(ndvi)
}

system.time(
		tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_nodrop)
)

# We can make this even easier using a data.frame as input, as long as we name
#	the image bands:

names(tahoe_highrez) <- c("Green","Red","NIR")

ndvi_dataframe <- function(GRNIR_image)
{
	# The input is now converted to a data.frame, and can be treated as such:
	red_band <- GRNIR_image$Red
	nir_band <- GRNIR_image$NIR
	
	ndvi <- (nir_band-red_band)/(nir_band + red_band)
	return(ndvi)
}

system.time(
		tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_dataframe,chunk_format="data.frame")
)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

# Notice that this now takes longer (running in sequential mode):
system.time(
		tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_nodrop)
)

##################################################################################################
##### C. Multiple input rasters #####
# rasterEngine can use functions that are applied
# to more than one raster, assuming that the
# rasters have the same number of ROWS and COLUMNS.
# Right now, rasterEngine does ZERO safety checks,
# so you should be certain ahead of time these
# rasters follow this requirement.

require("spatial.tools")

# Begin a parallel cluster and register it with foreach:
cpus = 12 # The number of nodes/cores to use in the cluster
# This can be modified to fit a specific cluster/multicore computer setup
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Load up a 2 images:
tahoe_lidar_highesthit <- setMinMax(
		raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools")))

tahoe_lidar_bareearth <- setMinMax(
		raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools")))

tahoe_lidar_bareearth
plot(tahoe_lidar_bareearth)

tahoe_lidar_highesthit
plot(tahoe_lidar_highesthit)

# This function will calculate the LIDAR height image by subtracting
# the bare earth surface from the first-return surface.
# Notice we simply have two variables in our function:

LIDAR_height <- function(bareearth,firstreturn)
{
	height <- firstreturn-bareearth
	return(height)
}

system.time(
		tahoe_height <- rasterEngine(
				bareearth=tahoe_lidar_bareearth,
				firstreturn=tahoe_lidar_highesthit,
				fun=LIDAR_height)
)

# We can also send it as a list of Rasters, making sure you assign the list to "x":
system.time(
		tahoe_height <- rasterEngine(
				x=list(
						bareearth=tahoe_lidar_bareearth,
						firstreturn=tahoe_lidar_highesthit),
				fun=LIDAR_height)
)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.
# This will run more slowly now that it is in sequential mode:
system.time(
		tahoe_height <- rasterEngine(
				bareearth=tahoe_lidar_bareearth,
				firstreturn=tahoe_lidar_highesthit,
				fun=LIDAR_height)
)

##################################################################################################
##### D. Multiple output rasters #####
# rasterEngine can output more than one raster.
# The functions must return a list of arrays, and 
# the filename parameter should be the same length
# as the length of the function's output list (since 
# each array will be written to a different file).
# If multiple output rasters are returned, rasterEngine
# will return them as a list of brick() objects.

require("spatial.tools")

# Begin a parallel cluster and register it with foreach:
cpus = 12 # The number of nodes/cores to use in the cluster
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Note: you can use a conveinence script to do the same thing:
?sfQuickInit

# Load up a 1-band image:
tahoe_lidar_highesthit <-
		setMinMax(raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools")))
tahoe_lidar_highesthit
plot(tahoe_lidar_highesthit)

# Our function will return two output files, one in which the
# input is converted to feet, and one in which the input is
# converted to kilometers.

meters_to_feet_and_kilometers <- function(elevation_in_meters)
{
# elevation_in_meters is received by this function as an array
	feet <- elevation_in_meters * 3.28084
	kilometers <- elevation_in_meters/1000
	
	# We must return these as a list:
	outputs <- list(feet,kilometers)
	
	return(outputs)
}

# Apply it to the input image using rasterEngine:
# Hint: run this twice to see the first-time run
# vs. subsequent runs. The first time the parallel
# engine is invoked, it has to load the packages into the workers.
# The second time, the packages are already loaded
# so the execution is faster.
system.time(
		tahoe_lidar_highesthit_feet_and_kilometers <-
				rasterEngine(
# Match the variable name in the function to the raster:
						elevation_in_meters=tahoe_lidar_highesthit,
# Assign the function:
						fun=meters_to_feet_and_kilometers,
# Give it two output filenames:
						filename=c("test_feet","test_kilometers")
				)
)

plot(tahoe_lidar_highesthit_feet_and_kilometers[[1]]) # In feet
plot(tahoe_lidar_highesthit_feet_and_kilometers[[2]]) # In km.

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

##################################################################################################
##### E. Using non-raster parameters in the model #####
# We can pass arguments to the function using the args=
# setting in rasterEngine:

require("spatial.tools")

# Begin a parallel cluster and register it with foreach:
cpus = 12 # The number of nodes/cores to use in the cluster
# This can be modified to fit a specific cluster/multicore computer setup
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Load up a 1-band image:
tahoe_lidar_highesthit <-
		setMinMax(raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools")))
tahoe_lidar_highesthit
plot(tahoe_lidar_highesthit)

# Our function will multiply the input raster times some parameter:
apply_multiplier <- function(inraster,multiplier)
{
# elevation_in_meters is received by this function as an array
	multiplied_raster <- inraster * multiplier
	return(multiplied_raster)
}

# Note that args= is a list, where the variable name must be specified:
system.time(
		tahoe_lidar_multiplied <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_lidar_highesthit,
# Assign the function:
						fun=apply_multiplier,
						args=list(multiplier=3.28084)
				)
)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.