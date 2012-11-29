\name{build_raster_header}
\alias{build_raster_header}
\title{build_raster_header}
\usage{
  build_raster_header(x_filename, reference_raster,
    out_nlayers, dataType = "FLT8S", format = "raster",
    bandorder = "BIP", verbose = FALSE)
}
\arguments{
  \item{x_filename}{Character. The filename of the input
  binary file.}

  \item{reference_raster}{Raster*. A Raster* object
  containing the header information to be used.}

  \item{out_nlayers}{Numeric. The number of layers in the
  flat binary file (defaults to
  nlayers(reference_raster)).}

  \item{dataType}{Character. The dataType of the flat
  binary file.  See ?dataType for available datatypes.
  Default is 'FLT8S'.}

  \item{bandorder}{Character. The bandorder
  ('BIP','BIL','BSQ') of the file. Default is 'BIP'.}

  \item{format}{Character. The format of the header.  See
  ?hdr for valid entries.  Default is 'raster'.  CURRENTLY
  UNSUPPORTED.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}
}
\description{
  Builds a raster header for a flat binary file.
}
\examples{
# TODO
}
\author{
  Jonathan A. Greenberg and Robert Hijimans
  (\email{spatial.tools@estarcion.net})
}
\seealso{
  \code{\link{hdr}},\code{\link{dataType}}
}

