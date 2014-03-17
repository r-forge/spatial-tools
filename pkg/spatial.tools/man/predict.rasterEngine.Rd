\name{predict.rasterEngine}
\alias{predict.rasterEngine}
\title{Model predictions on rasters}
\usage{
predict.rasterEngine(object, newdata, ...)
}
\arguments{
  \item{object}{a model object for which prediction is
  desired.}

  \item{newdata}{Raster*. Raster* objects to apply the
  model to.}

  \item{...}{additional arguments affecting the predictions
  produced.}
}
\description{
Model predictions on rasters
}
\details{
predict.rasterEngine will apply a model to a Raster*
object, following the basic approach outlined in the
generic predict.  Currently, this works for predict.*
statements in which the data to predict on is called by the
parameter "newdata", the input data is in the form of a
data.frame, and the output is a vector or matrix of numbers
or factors.

predict.rasterEngine will run in parallel if a cluster is
registered with foreach via a do* statement, or if the user
uses sfQuickInit().
}
\examples{
# TODO
}
\author{
Jonathan A. Greenberg (\email{spatial.tools@estarcion.net})
}
\seealso{
\code{\link{predict}}
}

