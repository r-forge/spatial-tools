lockfilename <- paste(tempdir(),".lock",sep="")

create_lockfile <- function(lockfilename,wait=0)
{
	lock_created <- FALSE
	while(!lock_created)
	{
		lock_created <- TRUE
		tryCatch(dir.create(lockfilename,recursive=TRUE),
				warning=function(err) lock_created <<- FALSE)
		Sys.sleep(wait)
	}
	return(TRUE)
}

remove_lockfile <- function(lockfilename)
{
	unlink(lockfilename,recursive=TRUE)
	return(TRUE)
}