packages_required <- c("spatial.tools","doParallel","randomForest")
lapply(packages_required, require, character.only=T)

# Begin a parallel cluster and register it with foreach:
#cpus = 12 # The number of nodes/cores to use in the cluster
## This can be modified to fit a specific cluster/multicore computer setup
#cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
## Register the cluster with foreach:
#registerDoParallel(cl)

# Load up a 3-band image:
tahoe_highrez <- setMinMax(
		brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))
#tahoe_highrez
#plotRGB(tahoe_highrez)

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

# tahoe_rf

tahoe_rf_class <- predict_rasterEngine(object=tahoe_rf,newdata=tahoe_highrez,type="response")


tahoe_highrez[tahoe_highrez>100] <- NA
tahoe_rf_class <- predict_rasterEngine(object=tahoe_rf,newdata=tahoe_highrez,type="prob")



### TIM BUGS
packages_required <- c("spatial.tools","doParallel","randomForest")
lapply(packages_required, require, character.only=T)

# Load up a 3-band image:
tahoe_highrez <- setMinMax(
		brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))
tahoe_highrez
plotRGB(tahoe_highrez)

#create a categorical layer from band 1
mat <- matrix(c(0,50,1,50,150,2,150,255,3),ncol=3,byrow=TRUE)
bnd1cat <- reclassify(tahoe_highrez[[1]], rcl=mat)
bnd1cat <- ratify(bnd1cat)
rat <- levels(bnd1cat)[[1]]
rat$types <- c('type1', 'type2', 'type3', 'type4')
rat$code <- c(1,2,3,4)
levels(bnd1cat) <- rat

#library(rasterVis) #if you want a categorical plot
#levelplot(bnd1_rc2)

tahoe_highrez <- addLayer(tahoe_highrez, bnd1cat)
names(tahoe_highrez) <- c("tahoeOne","tahoeTwo","tahoeThree","tahoeCatFour")


# Load up some training points:
tahoe_highrez_training_points <- readOGR(
		dsn=system.file("external", package="spatial.tools"),
		layer="tahoe_highrez_training_points")

# Extract data to train the randomForest model:
tahoe_highrez_training_extract <- extract(tahoe_highrez,tahoe_highrez_training_points,df=TRUE)

# Fuse it back with the SPECIES info:
tahoe_highrez_training_extract$SPECIES <- tahoe_highrez_training_points$SPECIES

# Note the names of the bands:
names(tahoe_highrez_training_extract) # the extracted data
names(tahoe_highrez) # the brick

# convert to factor, ensure all the levels are there
tahoe_highrez_training_extract$tahoeCatFour <- factor(tahoe_highrez_training_extract$tahoeCatFour)
levels(tahoe_highrez_training_extract$tahoeCatFour) <- c(1,2,3,4)
str(tahoe_highrez_training_extract)


# Generate a randomForest model:
tahoe_rf <- randomForest(y=tahoe_highrez_training_extract$SPECIES, 
		x=tahoe_highrez_training_extract[,2:5],
		data=tahoe_highrez_training_extract)


# try it with standard predict call -- this works                            
predict1_rf_prob <- predict(object=tahoe_highrez, model=tahoe_rf, type="prob")

# try it with rasterEngine -- this fails
sfQuickInit()
predict2_rf_prob <- predict_rasterEngine(object=tahoe_rf,newdata=tahoe_highrez,type="prob")
sfQuickStop()


setwd("/Users/jgrn307/Downloads")
