extract_enhanced_prechecks <- function(x,y,outformat,fun,verbose)
{
	# Coerce all inputs to a list.
	if(class(x) != "list") x <- list(x)	
	if(class(y) != "list") y <- list(y)
	
	# Check for names:
	if(is.null(names(x))) names(x) <- as.character(1:length(x))
	if(is.null(names(y))) names(y) <- as.character(1:length(y))
	
	if(is.null(fun) & outformat=="Spatial") outformat="data.frame"
	
	return(list(x=x,y=y,outformat=outformat))
}

extract_enhanced_reproj <- function(x,y,verbose)
{	
# Reproject the vectors ahead of time (memory danger!)
	x_proj <- sapply(x,projection)
	x_proj_unique <- lapply(unique(x_proj),function(x) x)
	
	extract_enhanced_reprojFunction <- function(y,x_proj_unique)
	{
		y_reproj <- lapply(x_proj_unique,
				function(x,y) spTransform(y,CRS(x)),
				y=y)
		names(y_reproj) <- x_proj_unique
		return(y_reproj)
	}
	
	y_reproj <- foreach(i=y,.packages=c("sp","raster")) %dopar%
			extract_enhanced_reprojFunction(i,x_proj_unique)
	return(y_reproj)
}

extract_enhanced_data.frame <- function(x,y_reproj,fun,verbose)
{
	extract_enhanced_one_raster <- function(raster_vector_combinations,
			x,y_reproj,verbose)
	{
		current_raster <- as.numeric(raster_vector_combinations[1])
		current_vector <- as.numeric(raster_vector_combinations[2])
		
		# Determine the vector ID with the matching projection.
		y_reproj_id <- which(names(y_reproj[[current_vector]])==projection(x[[current_raster]]))
		
		extracted_data <- extract(x=x[[current_raster]],
				y=y_reproj[[current_vector]][[y_reproj_id]],fun=fun,df=TRUE)
		
		return(extracted_data)
	}
	
	raster_vector_combinations <- expand.grid(list(seq(x),seq(y_reproj)))
	extracted_data_list <- foreach(raster_vector_combinations-raster_vector_combinations,
					.packages=c("raster"),.verbose=verbose) %dopar% 
			extract_enhanced_one_raster(raster_vector_combinations,x,y_reproj,verbose)
	
	return(extracted_data_list)
	
}

extract_enhanced_core <- function(raster,vector,
		outformat="Spatial",
		# extract() parameters
		fun=NULL,na.rm=FALSE,
		weights=FALSE,cellnumbers=FALSE,
		method='simple',buffer=NULL,small=FALSE,
		layer=1,nl=nlayers(raster),factors=FALSE,
		# Other settings
		verbose=FALSE)
{
	if(projection(vector)!=projection(raster))
	{
		vector_reproj <- spTransform(vector,CRS(projection(raster)))
	} else
	{
		vector_reproj <- vector
	}
	
	# Base extraction
	if(outformat=="Spatial" | outformat=="data.frames" | outformat=="matrix")
	{
		if(outformat=="matrix") df=FALSE else df=TRUE
		extracted_data <- extract(x=raster,y=vector_reproj,
				method=method,buffer=buffer,small=small,
				fun=fun,na.rm=na.rm,
				cellnumbers=cellnumbers,
				df=df,
				weights=weights,factors=factors,
				layer=layer,nl=nl)
#		if(class(vector_reproj)==)
		
		if(outformat=="matrix") return(extracted_data)
		
		if(outformat=="Spatial" | outformat=="data.frames")
		{
			if(class(vector_reproj)=="SpatialPointsDataFrame" |
					class(vector_reproj)=="SpatialLinesDataFrame" |
					class(vector_reproj)=="SpatialPolygonsDataFrame")
			{
				# Extract and merge with Spatial*DataFrame
				SpatialDataFrame <- vector_reproj@data
				extracted_data <- merge(SpatialDataFrame,extracted_data,all=TRUE)
				if(outformat=="Spatial")
				{
					outvector <- vector
					outvector@data <- extracted_data
					return(outvector)
				} else
				{
					return(extracted_data)
				}
			} else
			{
				if(outformat=="Spatial")
				{
					outvector <- vector
					outvector@data <- extracted_data
					return(outvector)
				} else
				{
					return(extracted_data)
				}
			}
			
			
		}
		
		
	}
	# Minirasters here
	
	
	return(extracted_data)
}



extract_enhanced <- function(x,y,
		outformat="Spatial",
		fun=NULL,na.rm=FALSE,weights=FALSE,
		cellnumbers=FALSE,factors=FALSE,layers=NULL,
		simplify=TRUE,
		verbose=FALSE,
		...)
{
	# Pre-checks
	list2env(extract_enhanced_prechecks(x,y,outformat,fun,verbose),envir=environment())
	
	# Set up reprojected vectors
	y_reproj <- extract_enhanced_reproj(x,y,verbose)
	
	# Perform the extraction based on the outformat:
	if(outformat=="data.frames") 
		extracted_data_list <- extract_enhanced_data.frame(x,y_reproj,fun,verbose)
	
	return(extracted_data_list)
}
require("spatial.tools")
x <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
y <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training_points_utm")
debug(extract_enhanced_core)
extract_enhanced_core(raster=x,vector=y)