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
detach("package:spatial.tools", unload=TRUE)
packages_required <- c("spatial.tools","doParallel","randomForest","raster","rgdal")
lapply(packages_required, require, character.only=T)

# Begin a parallel cluster and register it with foreach:
# cpus = 12 # The number of nodes/cores to use in the cluster
# This can be modified to fit a specific cluster/multicore computer setup
# cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
# registerDoParallel(cl)

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
#	require(randomForest)
	
# First, preserve the names:
#	band_names <- dimnames(inraster)[3][[1]]
	
# This will "flatten" the array to a matrix (we lose the names here):
#	inraster_matrix <- inraster
#	dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
	
# Now, let�s coerce this to a data.frame:
#	inraster.df <- as.data.frame(inraster_matrix)
	
# We need to re-set the names because randomForest requires it:
#	names(inraster.df) <- band_names
	
# Now we can use this in a predict statement:
	out_predictions <- predict(rfModel,inraster)
	
# We must return this as a numeric array (a raster cannot
# use character classes), and set the dims to match the input.
# Also, right now rasterEngine requires ALL outputs to be double
# precision floating point, so we cannot use "as.numeric" on the
# factor, because that will return an integer.
	
#	out_predictions_array <- array(as.double(out_predictions),dim=c(dim(inraster)[1:2],1))
	
	return(out_predictions)
}

# Now, rasterEngine. Notice we pass the randomForest model along
# via the args= setting.

detach("package:spatial.tools", unload=TRUE)
packages_required <- c("spatial.tools","doParallel","randomForest","raster","rgdal")
lapply(packages_required, require, character.only=T)

system.time(
		tahoe_lidar_rf_class <-
				rasterEngine(
# Match the variable name in the function to the raster:
						inraster=tahoe_highrez,
# Assign the function:
						fun=randomForest_raster_predict,
						args=list(rfModel=tahoe_rf),
						chunk_format="data.frame"
				)
)
tahoe_lidar_rf_class <- setMinMax(tahoe_lidar_rf_class)
tahoe_lidar_rf_class
plot(tahoe_lidar_rf_class)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.