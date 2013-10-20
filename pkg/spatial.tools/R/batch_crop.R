

batch_crop <- function(rasters,vector,each.vector=TRUE,preproject=TRUE,skip_no_projection=TRUE,
		output_directory,file_prefix="raster_crop_",format="GTiff")
{
	if(!is.list(rasters))
	{
		rasters <- list(rasters)
	}
		
	if(preproject)
	{
		unique_projections <- unique(foreach(raster=rasters,.packages="raster",.combine=c,.inorder=FALSE) %dopar%
						{
							raster_proj <- projection(raster)
							if(raster_proj=="NA") return(NULL) else return(raster_proj)
						})
		
		vector_reprojections <- foreach(unique_projection=unique_projections,.packages="rgdal") %dopar%
				{
					spTransform(vector,CRS(unique_projection))
				}
	}
	
	vector_n <- length(vector)
	rasters_n <- length(rasters)
	
	rasterEnvelopes <- foreach(raster=rasters,.packages=c("spatial.tools")) %dopar%
			{
				rasterEnvelope(raster)
			}
	
	rastersVectorIntersections <- foreach(rasterEnvelope=rasterEnvelopes,rasterID=seq(rasters_n),
					.packages="rgdal",.combine=rbind) %dopar%
			{
				# print(rasterEnvelope)
				current_CRS <- projection(rasterEnvelope)
				if(current_CRS=="NA")
				{
					if(skip_no_projection) return(NULL)
					else stop("raster has no projection information.  Stopping.")
				}
				
				if(!preproject)
				{
					vector_reprojection <- spTransform(vector,CRS(current_CRS))
				} else
				{
					vector_reprojection <- vector_reprojections[unique_projections==current_CRS][[1]]
				}
				
				intersecting_polys_check <- gCovers(rasterEnvelope,vector_reprojection,byid=TRUE)
				# print(sum(intersecting_polys_check))
				if(!any(intersecting_polys_check))
				{
					#	print("NAY")
					return(NULL)
				} else
				{
					intersecting_polys_ids <- seq(vector_n)[intersecting_polys_check]
					#	intersecting_polys <- vector_reprojection[intersecting_polys_ids,]
					intersecting_polys_ids_N <- length(intersecting_polys_ids)
					rasterVectorIntersections <- cbind(rep(rasterID,intersecting_polys_ids_N),intersecting_polys_ids)
					colnames(rasterVectorIntersections) <- c("rasterID","polyID")
				}
				return(rasterVectorIntersections)
			}
	
	rastersVectorIntersections_list <- lapply(seq(nrow(rastersVectorIntersections)),
			function(x,rastersVectorIntersections) return(rastersVectorIntersections[x,]),
			rastersVectorIntersections=rastersVectorIntersections)
	
	cropped_rasters <- foreach(rastersVectorIntersections=rastersVectorIntersections_list,.packages="raster") %dopar%
			{
				rasterID <- rastersVectorIntersections[1]
				vectorID <- rastersVectorIntersections[2]
				
				crop_filename <- file.path(output_directory,
						paste(file_prefix,
						"R",add_leading_zeroes(rasterID,max_number=rasters_n),
						"_V",add_leading_zeroes(vectorID,max_number=vector_n),
						sep=""))
				
				raster_to_be_cropped <- rasters[[rasterID]]
				
				current_CRS <- projection(raster_to_be_cropped)
				
				if(!preproject)
				{
					crop_vector <- spTransform(vector[vectorID,],CRS(current_CRS))
				} else
				{
					crop_vector <- vector_reprojections[unique_projections==current_CRS][[1]][vectorID,]
				}
				
				cropped_raster <- crop(raster_to_be_cropped,crop_vector,filename=crop_filename,format=format)
				return(cropped_raster)
			}
	return(cropped_rasters)
}