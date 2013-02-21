\name{focal_hpc}
\alias{focal_hpc}
\title{focal_hpc}
\usage{
  focal_hpc(x, fun, window_dims = c(1, 1), window_center,
    args = NULL, filename = "", overwrite = FALSE,
    outformat = "raster", processing_unit = "window",
    chunk_nrows = 20, chunk_format = "array",
    verbose = FALSE, ...)
}
\arguments{
  \item{x}{Raster*. A Raster* used as the input into the
  function.  Multiple inputs should be stacked together.}

  \item{fun}{function. A focal function to be applied to
  the image. See Details.}

  \item{window_dims}{Vector. The size of a processing
  window in col x row order.  Be default, a single pixel
  (c(1,1).}

  \item{window_center}{Vector. The local coordinate of the
  center of a processing window.  By default the middle of
  the processing window.}

  \item{processing_unit}{Character. Can be "window" or
  "chunk".  In general, leave it be and it will be auto-set
  by the function.}

  \item{chunk_nrows}{Numeric. The number of rows in each
  chunk, default = 20.}

  \item{chunk_format}{Character. The format to send the
  chunk to the function.  Can be "array" (default) or
  "raster".}

  \item{args}{list. Arguments to pass to the function (see
  ?mapply).}

  \item{filename}{character. Filename of the output
  raster.}

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
  focal_hpc is designed to execute a function on a Raster*
  object using foreach, which can achieve parallel reads,
  executions and writes. Parallel random writes are
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

  focal_hpc operates in two modes, which have different
  input and outputs to the function: Pixel based
  processing: 1) If chunk_format=="array" (default), the
  input to the function should assume an array of
  dimensions x,y,z where x = the number of columns in a
  chunk, y = the number of rows in the chunk, and z = the
  number of bands in the chunk.  If chunk_format=="raster",
  the input to the function will be a raster subset. Note
  that we are ordering the array using standards for
  geographic data, (columns, rows, bands), not how R
  usually thinks of arrays (rows, columns, bands).

  2) The output of the function should always be an array
  with the x and y dimensions matching the input, and an
  arbitrary number of band outputs.  Remember to order the
  dimensions as columns, rows, bands (x,y,z).

  Local window processing: 1) The function should be
  written to process a SINGLE window at a time, so the
  input to the function should assume a window of
  dimensions window_dims with a local center defined by
  window_center.  As with before, the input can be passed
  as an array (suggested) or a raster. 2) The output should
  be a single pixel value, so can either be a single value,
  or a vector (which is assumed to be

  but should always rely on a vector (if a single band
  input) or a matrix (if a multiband input).  getValues()
  is used to read a chunk of data that is passed to the
  function.  If the function relies on multiple, same-sized
  raster/brick inputs, they should first be coerced into a
  single stack(), and then the beginning of the function
  should deconstruct the stack (which will be received by
  the function as a matrix) back into individual components
  (see the example below).

  The speed of the execution will vary based on the
  specific setup, and may, indeed, be slower than a
  sequential execution (e.g. with calc() ).
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
	Pixel-based processing:
	ndvi_function <- function(x,...)
	{
		red_band <- inchunk[,,2]
		nir_band <- inchunk[,,3]
		ndvi <- (nir_band - red_band)/(nir_band + red_band)
		return(ndvi)
	}
 tahoe_ndvi <- focal_hpc(x=tahoe_highrez,fun=ndvi_function)
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}
\seealso{
  \code{\link{clusterMap}}, \code{\link{mmap}},
  \code{\link{dataType}}, \code{\link{hdr}}
}

