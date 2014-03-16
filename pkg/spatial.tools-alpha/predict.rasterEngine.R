# newdata will be the rasterdata
library(spatial.tools)

predict.rasterEngine(object,newdata,...)
{
	predict.rasterEngine_function <- function(newdata,object,...)
	{
		# Determine all parameters that are not newdata and object:
		local_objects <- ls()
		model_parameters <- setdiff(local_objects,c("newdata","object"))
		
	#	library(plyr)
		# Receives the chunk as an array, needs to coerce it to data.frame
		newdata_matrix <- aperm(newdata,c(3,2,1))
		dim(newdata_matrix) <- c(dim(newdata)[3],prod(dim(newdata)[1:2]))
		newdata_df <- as.data.frame(t(newdata_matrix))
		names(newdata_df) <- dimnames(newdata)[3][[1]]
		
		predict_mod <- predict
		
		f <- c(formals(predict_mod), unlist(mget(model_parameters)),unlist(alist(... = )))	
#	f <- c(formals(fun), unlist(alist(... = )))
		formals(predict_mod) <- f[!duplicated(names(f))]
		
		predict_output <- predict(object,newdata=newdata_df,mget(model_parameters))
		
#		dim(newdata_matrix) <- c(dim(new))
#		
	#	newdata_df <- as.data.frame(as.table(newdata))
		
		return(newdata)
		
		
		
	}
}

tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))

rasterEngine(newdata=tahoe_highrez,fun=predict.rasterEngine_function,debugmode=TRUE,args=list(testarg=1))


### FROM TUTORIAL
require(c("spatial.tools","doParallel))
require(“randomForest”)
# Begin a parallel cluster and register it with foreach:
cpus = 12 # The number of nodes/cores to use in the cluster
# This can be modified to fit a specific cluster/multicore computer setup
cl <- makeCluster(spec = cpus, type = “PSOCK”, methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Load up a 3-band image:
tahoe_highrez <- setMinMax(
		brick(system.file(“external/tahoe_highrez.tif”, package=”spatial.tools”)))
tahoe_highrez
plotRGB(tahoe_highrez)

# Load up some training points:
tahoe_highrez_training_points <- readOGR(
		dsn=system.file(“external”, package=”spatial.tools”),
		layer=”tahoe_highrez_training_points”)

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

randomForest_raster_predict <- function(inraster,rfModel,…)
{
# We need to load randomForest (note this is only
# loaded once per worker on most cluster setups):
	require(randomForest)
	
# First, preserve the names:
	band_names <- dimnames(inraster)[3][[1]]
	
# This will “flatten” the array to a matrix (we lose the names here):
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
# precision floating point, so we cannot use “as.numeric” on the
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