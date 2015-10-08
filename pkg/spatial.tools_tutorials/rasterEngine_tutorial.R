# rasterEngine Tutorial
# Jonathan A. Greenberg
# jgrn@illinois.edu

# Last updated: 16 March 2014

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
# environment. Because rasterEngine uses ‘foreach’, a large
# number of possible parallel backends can be used, including:
# -parallel (built into R)
# -multicore (built into R)
# -snow
# -MPI (via Rmpi)

# rasterEngine provides both pixel-based and focal-based (local
# neighborhood) processing. rasterEngine will read, process,
# and write chunks of the output in parallel.

# The general process for using rasterEngine is:
# 1) Import Raster*’s of the input data using the ‘raster’ package.
# 2) Start a parallel backend using functions provided by, e.g.,
# the ‘parallel’ or ‘Rmpi’ packages.
# 3) Register the parallel backed with the foreach engine using
# a registerDo*() function (e.g. registerDoParallel())
# 4) Develop a function to be applied to a chunk of pixel data
# or a single local window.
# 5) Run rasterEngine:
# output <- rasterEngine(input=inputRaster,fun=chunkFunction,
# window_dims=c(window_x,window_y))
# 6) (Once finished) Shut down parallel backend.

# Acknowledgements: special thanks to Robert Hijimans (‘raster’)
# Jeffrey Ryan (‘mmap’) and Steve Weston (‘foreach’).

########## II. Getting rasterEngine ##########
# rasterEngine is part of the spatial.tools package, which is
# available on CRAN. However, this tutorial expects version 1.1.1 or later
# of spatial.tools. If it is not available on CRAN:
install.packages("spatial.tools")
# The user should get the latest version from R-forge:
install.packages("spatial.tools", repos="http://R-Forge.R-project.org",type="source")

##################################################################################################
########## III. Pixel-based processing ##########
# When a function is to be applied one pixel at a time (vs. a local neighborhood),
# rasterEngine provides the input data in an array format representing a chunk
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
# so we don’t need to manually set the dim(ndvi) at the
# end:
	red_band <- GRNIR_image[,,2,drop=FALSE]
	nir_band <- GRNIR_image[,,3,drop=FALSE]
	
	ndvi <- (nir_band-red_band)/(nir_band + red_band)
	return(ndvi)
}

system.time(
		tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_nodrop)
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

##################################################################################################
##### F. Using predict() with rasterEngine #####
# Typically, classification/continuous variable models
# are developed and then applied one band at a time.
# Because the function receives the pixel data as an
# array, and predict often wants a matrix or
# data.frame, we must first have our function convert
# the array of pixels to a data.frame. Also, the
# function should contain the library/require statement
# for any packages it might need.

# This is a trick for loading multiple packages at once, from
# http://stackoverflow.com/questions/8175912/load-multiple-packages-at-once
packages_required <- c("spatial.tools","doParallel","randomForest")
lapply(packages_required, require, character.only=T)

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

# Load up some training points:
tahoe_highrez_training_points <- readOGR(
		dsn=system.file("external", package="spatial.tools"),
		layer="tahoe_highrez_training_points")

# Extract data to train the randomForest model:
tahoe_highrez_training_extract <- extract(
		tahoe_highrez,
		tahoe_highrez_training_points,
		df=TRUE)

# Fuse it back with the SPECIES info:
tahoe_highrez_training_extract$SPECIES <- tahoe_highrez_training_points$SPECIES

# Note the names of the bands:
names(tahoe_highrez_training_extract) # the extracted data
names(tahoe_highrez) # the brick

# Generate a randomForest model:
tahoe_rf <- randomForest(SPECIES~tahoe_highrez.1+tahoe_highrez.2+tahoe_highrez.3,
		data=tahoe_highrez_training_extract)

tahoe_rf

# This model will be passed along as a parameter to our function.

randomForest_raster_predict <- function(inraster,rfModel)
{
# We need to load randomForest (note this is only
# loaded once per worker on most cluster setups):
	require(randomForest)
	
# First, preserve the names:
	band_names <- dimnames(inraster)[3][[1]]
	
# This will "flatten" the array to a matrix (we lose the names here):
	inraster_matrix <- inraster
	dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
	
# Now, let’s coerce this to a data.frame:
	inraster.df <- as.data.frame(inraster_matrix)
	
# We need to re-set the names because randomForest requires it:
	names(inraster.df) <- band_names
	
# Now we can use this in a predict statement:
	out_predictions <- predict(rfModel,inraster.df)
	
# We must return this as a numeric array (a raster cannot
# use character classes), and set the dims to match the input.
# Also, right now rasterEngine requires ALL outputs to be double
# precision floating point, so we cannot use "as.numeric" on the
# factor, because that will return an integer.
	
	out_predictions_array <- array(as.double(out_predictions),dim=c(dim(inraster)[1:2],1))
	
	return(out_predictions_array)
}

# Now, rasterEngine. Notice we pass the randomForest model along
# via the args= setting.
system.time(
		tahoe_lidar_rf_class <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_highrez,
# Assign the function:
						fun=randomForest_raster_predict,
						args=list(rfModel=tahoe_rf)
				)
)
tahoe_lidar_rf_class <- setMinMax(tahoe_lidar_rf_class)
tahoe_lidar_rf_class
plot(tahoe_lidar_rf_class)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

##################################################################################################
########## IV. Focal Window Functions ##########
# Focal functions expect a different input and output than pixel based functions.
# A focal function should expect an array representing a SINGLE neighborhood to be
# passed to the function, and a SINGLE pixel value to be returned. rasterEngine
# requires the user to supply the window size in x and y pixel coordinates using
# the window_dims=c() argument.

##################################################################################################
##### A. Single input raster (1-band) focal function #####

# Required packages. doParallel can be replaced with other
# foreach backends, e.g. doMPI, doSNOW, etc.
# Reference: http://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf

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

# Our function will perform a mean smoothing on the input data.
# A note with writing local window functions — they MUST be able
# to handle NA values properly. Edge and corner neighborhoods will contain
# NAs when passed to the function.

mean_smoother <- function(inraster) 
{
# We can use apply to
	smoothed <- mean(inraster)
	return(smoothed)
}

# Test the function on an array:
test_input <- array(1:9,dim=c(3,3,1))
test_input
mean_smoother(inraster=test_input)

# Apply it to the input image using rasterEngine:
# Hint: run this twice to see the first-time run
# vs. subsequent runs. The first time the parallel
# engine is invoked, it has to load the packages into the workers.
# The second time, the packages are already loaded
# so the execution is faster.
system.time(
		tahoe_lidar_highesthit_3x3_smoothed <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_lidar_highesthit,
# Assign the function:
						fun=mean_smoother,
						window_dims=c(3,3) # ncol = 3, nrow = 3 in the neighborhood
				)
)

plot(tahoe_lidar_highesthit_3x3_smoothed)

# We can easily modify the window size and re-run this function:
system.time(
		tahoe_lidar_highesthit_7x7_smoothed <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_lidar_highesthit,
# Assign the function:
						fun=mean_smoother,
						window_dims=c(7,7) # Note the larger window
				)
)
plot(tahoe_lidar_highesthit_7x7_smoothed)
# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

##################################################################################################
##### B. Single input raster (Multiple bands) focal function #####
# Multiple bands for use with a focal function probably require the use
# of an apply(…,3) statement in the function to work properly. In this
# example, we want to smooth each band of an input raster and returns all
# 3 bands.

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

mean_multiband_smoother <- function(inraster) 
{
# This time, because we have a multiple
# band array, if we use mean(inraster),
# it will return the average of ALL values
# within the neighborhood across all bands.
# Instead, we want to return a vector of values,
# one per band, so we use apply against the third
# dimension:
	smoothed <- apply(inraster,3,mean)
	return(smoothed)
}

system.time(
		tahoe_highrez_3x3_smoothed <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_highrez,
# Assign the function:
						fun=mean_multiband_smoother,
						window_dims=c(3,3) # ncol = 3, nrow = 3 in the neighborhood
				)
)
plotRGB(tahoe_highrez_3x3_smoothed)
# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

##################################################################################################
##### D. Accelerated focal functions #####

# Required packages. doParallel can be replaced with other
# foreach backends, e.g. doMPI, doSNOW, etc.
# Reference: http://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf

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

# Our function will perform a mean smoothing on the input data.
# A note with writing local window functions — they MUST be able
# to handle NA values properly. Edge and corner neighborhoods will contain
# NAs when passed to the function.

mean_smoother <- function(inraster) 
{
# We can use apply to
	smoothed <- mean(inraster)
	return(smoothed)
}

# Test the function on an array:
test_input <- array(1:9,dim=c(3,3,1))
test_input
mean_smoother(inraster=test_input)

# Apply it to the input image using rasterEngine:
# Hint: run this twice to see the first-time run
# vs. subsequent runs. The first time the parallel
# engine is invoked, it has to load the packages into the workers.
# The second time, the packages are already loaded
# so the execution is faster.
system.time(
		tahoe_lidar_highesthit_3x3_smoothed <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_lidar_highesthit,
# Assign the function:
						fun=mean_smoother,
						window_dims=c(3,3),
						debugmode=TRUE,
						processing_unit="chunk"# ncol = 3, nrow = 3 in the neighborhood
				)
)

plot(tahoe_lidar_highesthit_3x3_smoothed)

# We can easily modify the window size and re-run this function:
system.time(
		tahoe_lidar_highesthit_7x7_smoothed <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_lidar_highesthit,
# Assign the function:
						fun=mean_smoother,
						window_dims=c(7,7) # Note the larger window
				)
)
plot(tahoe_lidar_highesthit_7x7_smoothed)
# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.

















##################################################################################################
##### V. Debugging Functions #####

# Because foreach is spawning the function on remote nodes,
# standard debug() or even various print() statements may
# not work while running a cluster. To debug a function,
# I recommend one of two approaches:
# 1) Test chunk approach: extract a chunk of data, test/debug
# the function on the chunk, and once it is confirmed
# to work, then implement it on the image.
# 2) Lots of print statements & sequential mode.

### Test chunk approach ###
# Note, you should be familiar with:
?debug

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))

# We can test a broken version of our ndvi function:
ndvi_broken <- function(GRNIR_image,…)
{
# The input array will have dim(GRNIR_image)[3] equal
# to 3, because the input image had three bands.
# Note: the following two lines now return an array,
# so we don’t need to manually set the dim(ndvi) at the
# end:
	red_band <- GRNIR_image[,,2]
	nir_band <- GRNIR_image[,,3]
	
	ndvi <- (nir_band – red_band)/(nir_band + red_band)
	return(ndvi)
}

# We’ll try this in rasterEngine (suggestion: run this in sequential model
# to avoid workers becoming zombies):
stopCluster(cl) # Stops the cluster
registerDoSEQ()

ndvi_output <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_broken)
# Notice the error says the output wasn’t an array. We’ll need to fix this.

# rasterEngine uses (for single images) the function getValuesBlock_enhanced()
# to extract chunks to pass along to the function. As such, this
# function can be used directly to simulate a worker processing a single chunk:
?getValuesBlock_enhanced

# We will extract the first row, all columns:
GRNIR_chunk <- getValuesBlock_enhanced(tahoe_highrez,r1=1,r2=1)

# Note the class and dims of the input:
class(GRNIR_chunk)
dim(GRNIR_chunk)

test_chunk_output <- ndvi_broken(GRNIR_image=GRNIR_chunk)
class(test_chunk_output)
# Notice it is a numeric vector, not the array of dim 400,1,1 like it should be.

# We can troubleshoot the function line by line by:
debug(ndvi_broken)
test_chunk_output <- ndvi_broken(GRNIR_image=GRNIR_chunk)
# Hit enter or type n at the debug command line
# to go to the next line. Type Q and enter to
# exit from the debug.

# hit enter, notice Browse (the debug mode) shows the NEXT line
# it will run, so you should see:
# debug at #8: red_band <- GRNIR_image[, , 2]

# hit enter (the previous line will run and you can now check it):
class(red_band)
# Oops, this is numeric. We’ll fix it with a drop=FALSE:
red_band <- GRNIR_image[,,2,drop=FALSE]
class(red_band)
# Good, it is the right class, but what about the dim?
dim(red_band)
# Good!

# hit enter to move to the next line.
class(nir_band)
# Same error, let’s fix this:
nir_band <- GRNIR_image[,,3,drop=FALSE]
class(nir_band)
dim(nir_band)
# That is correct.
# Now hit enter which will execute:
# debug at #11: ndvi <- (nir_band – red_band)/(nir_band + red_band)
# Check ndvi:
class(ndvi)
dim(ndvi)

# Both of these are correct. Remember the output array needs to have
# the same number of columns and rows as the inputs:
dim(red_band)[1:2]
dim(ndvi)[1:2]
# The third dimension (the number of bands in the output) is arbitrary.
# Now that we know what has to be fixed, we’ll exit out of our
# debugging:
Q

# Now edit the function with the fixes:
ndvi_broken <- function(GRNIR_image,…)
{
	red_band <- GRNIR_image[,,2,drop=FALSE]
	nir_band <- GRNIR_image[,,3,drop=FALSE]
	
	ndvi <- (nir_band – red_band)/(nir_band + red_band)
	return(ndvi)
}

test_chunk_output <- ndvi_broken(GRNIR_image=GRNIR_chunk)
class(test_chunk_output)
dim(test_chunk_output)

# Confirmed. Now we can try it out on the whole image:
ndvi_output <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_broken)

### Print statement approach ###
# Note that this REQUIRES you run rasterEngine with a sequential
# backend, because the print statements are run on the workers.
# If you run it with a parallel backend, the print statements
# are run "invisibly":

stopCluster(cl) # Stops the cluster
registerDoSEQ()

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))

# We will simply add in some diagnostic print()
# statements to help us troubleshoot:
ndvi_broken <- function(GRNIR_image,…)
{
	red_band <- GRNIR_image[,,2]
	print("red band:")
	print(class(red_band))
	print(dim(red_band))
	
	nir_band <- GRNIR_image[,,3]
	print("nir band:")
	print(class(nir_band))
	print(dim(nir_band))
	
	ndvi <- (nir_band – red_band)/(nir_band + red_band)
	print("ndvi output:")
	print(class(ndvi))
	print(dim(ndvi))
	
	return(ndvi)
}

ndvi_output <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_broken)
# Notice in the output we see the red band class is wrong, and dim(red_band) is
# NULL because numeric vectors have no dim attribute:
dim(1:10)

# We would now use this to troubleshoot the function.

