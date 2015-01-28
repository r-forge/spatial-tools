

spArray_to_data.frame <- function(x)
{
	band_names <- dimnames(x)[[3]]
	x <- as.data.frame(x)
	names(x) <- band_names
	return(x)
}

