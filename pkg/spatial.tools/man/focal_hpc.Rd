\name{focal_hpc}
\alias{focal_hpc}
\title{focal_hpc}
\usage{
  focal_hpc(x, fun, window_dims, window_center,
    args = NULL, filename = "", cl = NULL,
    disable_cl = FALSE, overwrite = FALSE,
    outformat = "raster", verbose = FALSE,
    processing_unit = "window")
}
\arguments{
  \item{x}{Raster*. A Raster* used as the input into the
  function.  Multiple inputs should be stacked together.}

  \item{fun}{function. A focal function to be applied to
  the image. The input to the function is an array
  representing a local neighborhood to be processed, and
  the outputs of the function should be a single pixel
  vector (see Details).}

  \item{window_dims}{Vector. TBD.}

  \item{window_center}{Vector. TBD.}

  \item{processing_unit}{Character. TBD.}

  \item{args}{list. Arguments to pass to the function (see
  ?mapply).}

  \item{filename}{character. Filename of the output
  raster.}

  \item{cl}{cluster. A cluster object. calc_hpc will
  attempt to determine this if missing.}

  \item{disable_cl}{logical. Disable parallel computing?
  Default is FALSE.}

  \item{outformat}{character. Outformat of the raster. Must
  be a format usable by hdr(). Default is 'raster'.
  CURRENTLY UNSUPPORTED.}

  \item{overwrite}{logical. Allow files to be overwritten?
  Default is FALSE.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}
}
\description{
  Performs focal raster calculations using a snowfall
  cluster.
}
\details{
  TODO. focal_hpc is designed to execute a function on a
  Raster* object using a snowfall cluster to achieve
  parallel reads, executions and writes. Parallel random
  writes are achieved through the use of mmap, so
  individual image chunks can finish and write their
  outputs without having to wait for all nodes in the
  cluster to finish and then perform sequential writing.
  On Windows systems, random writes are possible but
  apparently not parallel writes.  calc_hpc solves this by
  trying to write to a portion of the image file, and if it
  finds an error (a race condition occurs), it will simply
  retry the writes until it successfully finishes.  On a
  Linux system, truly parallel writes should be possible.

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
# TODO
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}
\seealso{
  \code{\link{clusterMap}}, \code{\link{mmap}},
  \code{\link{dataType}}, \code{\link{hdr}}
}

