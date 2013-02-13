\name{sfQuickInit}
\alias{sfQuickInit}
\title{sfQuickInit}
\usage{
  sfQuickInit(cpus, ...)
}
\arguments{
  \item{...}{parameters to pass to sfInit()}
}
\description{
  Quickly initializes a parallel snowfall cluster.
}
\details{
  (Even more) quickly start a snow cluster with maximum
  available cpus, parallel = TRUE, and type = "SOCK" and
  registers it with foreach.
}
\examples{
\dontrun{
sfQuickInit()
sfGetCluster()
sfStop()
}
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[snowfall]{sfInit}}
}

