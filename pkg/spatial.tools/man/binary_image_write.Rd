\name{binary_image_write}
\alias{binary_image_write}
\title{binary_image_write
Writes image data to a flat binary file using col/row/band positioning.}
\usage{
  binary_image_write(filename, mode = real64(), image_dims,
    interleave = "BSQ", data, data_position)
}
\arguments{
  \item{filename}{Character.  The path and filename of a
  "blank" binary file to store the image data.}

  \item{mode}{The mode of data on disk.  Defaults to
  real64() (double precision floating point).}

  \item{image_dims}{Vector. Vector of length(image_dims)==3
  representing the number of columns, rows and bands in the
  output image.}

  \item{interleave}{Character. The require output
  interleave.  By default is "BSQ". OTHER INTERLEAVES
  CURRENTLY UNSUPPORTED.}

  \item{data}{Vector, matrix, array, or other data source
  which is coercible to a vector. This is the data to be
  written to the image.}

  \item{data_position}{TBD}
}
\value{
  NULL
}
\description{
  binary_image_write Writes image data to a flat binary
  file using col/row/band positioning.
}
\examples{
# TODO
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[mmap]{mmap}}
}
\keyword{mmap}

