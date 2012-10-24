\name{calc_hpc}
\alias{calc_hpc}
\title{calc_hpc}
\usage{
  calc_hpc(x, fun, args = NULL, filename = "", cl = NULL,
    disable_cl = FALSE, overwrite = FALSE,
    outformat = "raster", verbose = FALSE)
}
\arguments{
  \item{x}{Raster*. A Raster* used as the input into the
  function.  Multiple inputs should be stacked together.}

  \item{fun}{function. A function to be applied to the
  image. The input and outputs of the function should be a
  vector or matrix (see Details).}

  \item{args}{list. Arguments to pass to the function (see
  ?mapply).}

  \item{filename}{character. Filename of the output
  raster.}

  \item{cl}{cluster. A cluster object. calc_hpc will
  attempt to determine this if missing.}

  \item{disable_cl}{logical. Disable parallel computing?
  Default is FALSE.}

  \item{outformat}{character. Outformat of the raster. Must
  be a format usable by hdr(). Default is 'raster'.}

  \item{overwrite}{logical. Allow files to be overwritten?
  Default is FALSE.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}
}
\description{
  Performs raster calculations using a snowfall cluster.
}
\details{
  calc_hpc is designed to execute a function on a Raster*
  object using a snowfall cluster to achieve parallel
  reads, executions and writes. Parallel random writes are
  achieved through the use of mmap, so individual image
  chunks can finish and write their outputs without having
  to wait for all nodes in the cluster to finish and then
  perform sequential writing.  On Windows systems, random
  writes are possible but apparently not parallel writes.
  calc_hpc solves this by trying to write to a portion of
  the image file, and if it finds an error (a race
  condition occurs), it will simply retry the writes until
  it successfully finishes.  On a Linux system, truly
  parallel writes should be possible.

  The functions can be arbitrarily complex, but should
  always rely on a vector (if a single band input) or a
  matrix (if a multiband input).  getValues() is used to
  read a chunk of data that is passed to the function.  If
  the function relies on multiple, same-sized raster/brick
  inputs, they should first be coerced into a single
  stack(), and then the beginning of the function should
  deconstruct the stack (which will be received by the
  function as a matrix) back into individual components
  (see the example below).

  The speed of the execution will vary based on the
  specific setup, and may, indeed, be slower than a
  sequential execution (e.g. with calc() ).
}
\examples{
sfInit(parallel=TRUE,cpus=1)
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
ndvi_function <- function(x,red_band,NIR_band) {
# This is how to work with individual bands from a stack/brick in a function:
	red=x[,red_band]
	NIR=x[,NIR_band]
	ndvi = (NIR-red)/(NIR+red)
	return(ndvi)
}
# Arguments should be in a list, just like mapply requires.
ndvi_args=list(red_band=2,NIR_band=3)
# Sequential execution.
system.time(calc_hpc(x=tahoe_highrez,fun=ndvi_function,args=ndvi_args,
	overwrite=TRUE,filename="testndvi_seq",disable_cl=TRUE,verbose=FALSE))
# Parallel execution.
system.time(calc_hpc(x=tahoe_highrez,fun=ndvi_function,args=ndvi_args,
	overwrite=TRUE,filename="testndvi3_par",disable_cl=FALSE,verbose=FALSE))
sfStop()
}
\author{
  Jonathan A. Greenberg, Pritam Sukumar, and Robert
  Hijimans (\email{spatial.tools@estarcion.net})
}
\seealso{
  \code{\link{clusterMap}}, \code{\link{mmap}},
  \code{\link{dataType}}, \code{\link{hdr}}
}

