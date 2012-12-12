\name{fillNA}
\alias{fillNA}
\title{fillNA}
\usage{
  fillNA(x, stopmask = NULL, maxiter = 1000,
    verbose = FALSE, disable_cl = FALSE, ...)
}
\arguments{
  \item{x}{Raster*. Raster* object containing NA values to
  be filled in.}

  \item{stopmask}{Raster*. (Optional) A Raster* mask with
  0s at locations x should be filled in, and 1s for
  locations that should be left alone.}

  \item{maxiter}{Numeric. The algorithm is iterative, so if
  you want to have it stop after a certain number of
  iterations, set the value here.  Defaults to 1000.}

  \item{disable_cl}{logical. Disable parallel computing?
  Default is FALSE.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}

  \item{...}{Other parameters to pass to}
}
\description{
  Fill in NA regions from adjacent regions.
}
\examples{
# TODO
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}

