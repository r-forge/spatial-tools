#' Spatially Sync Rasters
#' 
#' Aligns ("syncs") a Raster to a reference Raster.
#' 
#' Uses bilinear or nearest neighbor resampling to align a raster to the extent
#' and projection of a reference raster and match the resolution of the
#' reference raster.  This is helpful in preparing multiple files of different
#' projections, resolutions, extents, and rotations for performing map algebra or 
#' change detection.
#' @name spatial_sync_raster
#' @title spatial_sync_raster
#' @param unsynced A Raster object to be aligned to the reference raster.
#' @param reference A Raster object to be used as the reference for syncing.
#' Syncing will use the reference's projection, resolution, and extent.
#' @param method Method used to compute values for the new RasterLayer. Either
#' 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation).
#' @param verbose verbose=TRUE gives feedback on the process (UNSUPPORTED AT
#' PRESENT).
#' @param size_only TODO
#' @param raster_size TODO
#' @return Returns a RasterLayer, RasterBrick or RasterStack object synced to
#' the reference raster object.
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @export
spatial_sync_raster <- function(unsynced,reference,method="ngb",size_only=FALSE,raster_size,verbose=FALSE)
{
	
	if(!size_only)
	{
		new_projection=projection(reference)
		old_projection=projection(unsynced)
		
		new_res=res(reference)
		old_res=res(unsynced)
		
		# Check for rotation
		new_extent=bbox(reference)
		old_extent=bbox(unsynced)
		
		if((new_extent[1,1] < 0 && old_extent[1,1] >=0) 
			|| (new_extent[1,1] >= 0 && old_extent[1,1] <0))
		{
			if(verbose) { print ("Rotating...") }
			unsynced_rotated=rotate(unsynced)
		} else
		{
			unsynced_rotated=unsynced
		}
		
		if(new_projection!=old_projection | new_res[1] != old_res[1] | new_res[2] != old_res[2])
		{
			pr_extent=projectExtent(unsynced_rotated, new_projection)
			# We need to fix the extent
			pr_extent <- setExtent(pr_extent,extent(reference))
			res(pr_extent)=res(reference)
			if(new_projection!=old_projection)
			{
				if(verbose) { print("Projecting and resampling...") }
				pr <- projectRaster(unsynced_rotated, pr_extent,method=method)
			} else
			{
				if(verbose) { print("Same projection, resampling only...") }
				pr <- raster::resample(unsynced_rotated, pr_extent,method=method)
			}
		} else
		{
			if(verbose) { print("Same projection and pixel size...") }
			pr=unsynced_rotated
		}
		
		if(verbose) { print("Expanding...") }
		expanded_raster=expand(pr,reference)
		if(verbose) { print("Cropping...") }
		synced_raster=crop(expanded_raster,reference)
	
		# This in theory shouldn't be neccessary...
		if(verbose) { print("Fixing extents...") }
		extent(synced_raster)=extent(reference)
	} else
	{
		if(missing(raster_size))
		{
			print("For size_only=TRUE you must set the raster_size as c(ncol,nrow)")
			return()
		} 
		
		unsynced_ncol=ncol(unsynced)
		unsynced_nrow=nrow(unsynced)
		
		# Eventually we should preserve the pixel size		
		unsynced_ulx=(raster_size[[1]]-unsynced_ncol)/2
		unsynced_uly=(raster_size[[2]]-unsynced_nrow)/2
		
		extent(unsynced)=extent(unsynced_ulx,unsynced_ulx+unsynced_ncol,unsynced_uly,unsynced_uly+unsynced_nrow)
		full_extent=extent(0,raster_size[[1]],0,raster_size[[2]])
		
		synced_raster=expand(unsynced,full_extent)
		extent(synced_raster)=full_extent
		res(synced_raster)=c(1,1)
	}

	return(synced_raster)
	
}