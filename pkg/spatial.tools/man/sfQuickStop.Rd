\name{sfQuickStop}
\alias{sfQuickStop}
\title{Quickly stops a parallel snowfall cluster and deregisters it from foreach.}
\usage{
  sfQuickStop(kill = FALSE, ...)
}
\arguments{
  \item{kill}{Logical. Force-kill parallel cluster.}

  \item{...}{parameters to pass to sfStop()}
}
\description{
  Quickly stops a parallel snowfall cluster and deregisters
  it from foreach.
}
\details{
  (Even more) quickly stop a snowfall cluster and sets
  foreach back to sequential mode via registerDoSEQ().
}
\examples{
sfQuickInit(cpus=2)
sfQuickStop()
}
\author{
  Jonathan A. Greenberg
}

