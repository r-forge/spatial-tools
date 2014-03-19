\name{predict_rasterEngine}
\alias{predict_rasterEngine}
\title{Model predictions (including Raster* objects)}
\usage{
predict_rasterEngine(object, ...)
}
\arguments{
  \item{object}{a model object for which prediction is
  desired.}

  \item{...}{additional arguments affecting the predictions
  produced.}
}
\description{
Model predictions (including Raster* objects)
}
\details{
predict will operate normally, unless a parameter named
"newdata" is found and it is of class Raster*.  If this
occurs, predict will use rasterEngine to perform a
prediction.  Currently, this works for predict.* statements
in which the data to predict on is called by the parameter
"newdata", the input data is in the form of a data.frame,
and the output is a vector or matrix of numbers or factors.

predict will run in parallel if a cluster is registered
with foreach via a do* statement, or if the user uses
sfQuickInit().
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

