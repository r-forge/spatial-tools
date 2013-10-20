


spSplit <- function(x,nsplits=2,splitProb=c(0.5,0.5),splitCol=NULL)
{
	if(is.null(splitCol))
	{
		stop("You must assign a column to split on (splitCol)")
	}
	
	if(!(splitCol %in% names(x)) & is.character(splitCol))
	{
		stop("splitCol not found in names(x)")
	}
	
	if(is.character(splitCol))
	{
		splitCol <- which(names(x)==splitCol)
	}
	
	if(length(splitProb) != nsplits)
	{
		stop("length(splitProb) must equal nsplits")
	}
	
#	if(sum(splitProb) != 1)
#	{
#		stop("sum(splitProb) must equal 1")
#	}
	
	spData <- as.data.frame(x)
	spStrataCol <- as.factor(spData[,splitCol])
	strata <- levels(spStrataCol)
	strataN <- table(spStrataCol)
	strataN <- strataN[strata]
	
	strata_samples <- sapply(strataN,function(x,splitProb) { floor(x*splitProb) },splitProb=splitProb)
	print(strataN)
	print(strata_samples)
	
	# Make sure there are enough data to split on, otherwise exit.
	
	if(!all(strataN>=nsplits))
	{
		stop("Not enough data in all columns to split on.")
	}
	
	strata_ids <- unlist(
			sapply(1:length(strata),
					function(x,strata_samples,strataN)
					{ 
						temp_strata_samples <- strata_samples[,x]
						temp_strataN=strataN[x]
						temp_ids <- unlist(
								sapply(X=(1:length(temp_strata_samples)),
										FUN=function(X,temp_strata_samples)
										{
											#	print(x)
											rep.int(X,temp_strata_samples[X])
										},temp_strata_samples)
						)
						if(length(temp_ids) < temp_strataN) temp_ids <- c(temp_ids,rep.int(NA,(temp_strataN-length(temp_ids))))
						temp_strata_samples <- sample(temp_ids,size=temp_strataN)
						return(temp_strata_samples)
					},
					strata_samples=strata_samples,strataN=strataN
			)
	)
	
	strataIndices <- unlist(sapply(strata,
					FUN=function(x,spStrataCol) { return(which(spStrataCol==x)) },
					spStrataCol=spStrataCol,simplify=FALSE))
	
	strata_ids_sorted <- strata_ids[order(strataIndices)]
	
	spSplits <- sapply(X=1:nsplits,FUN=
					function(X,strata_ids_sorted,spx)
			{
				strata_ids_split <- strata_ids_sorted==X
				strata_ids_split[is.na(strata_ids_split)] <- FALSE
				return(spx[strata_ids_split,])
			},strata_ids_sorted=strata_ids_sorted,
			spx=x
	)
	return(spSplits)
}

#x <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training_points")
#
#moo <- spSplit(x,splitCol="SPECIES",nsplits=3)