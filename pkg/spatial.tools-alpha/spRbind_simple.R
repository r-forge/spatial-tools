

spRbind_simple <- function(spList,CRS=1)
{
	if(is.numeric(CRS))
	{
		base_CRS <- CRS(proj4string(spList[[CRS]]))
	}
	
	# spTransform inputs
	
	spList_transformed <- foreach(spObj=spList,.packages="rgdal") %dopar%
			{
				spTransform(spObj,base_CRS)
			}
	
	rownames_all <- foreach(sp=spList_transformed,.packages="sp") %dopar%
			{
				rownames(as(sp,"data.frame"))
				
			}
	
	splength <- lapply(rownames_all,length)
	sp_end_id <- cumsum(unlist(splength))
	sp_begin_id <- c(1,sp_end_id[-length(sp_end_id)]+1)
	
	unique_ids <- lapply(seq(length(rownames_all)),
			function(x,sp_begin_id,sp_end_id)
			{
				sp_begin_id[x]:sp_end_id[x]
				
			},sp_begin_id=sp_begin_id,sp_end_id=sp_end_id)
	
	corrected_spFIDs <- mapply(function(spObj,unique_id)
			{
				spChFIDs(spObj,as.character(unique_id))
				
			},spObj=spList_transformed,unique_id=unique_ids)
	
	merged_sp <- do.call(rbind,corrected_spFIDs)
	return(merged_sp)
	
	
	
}