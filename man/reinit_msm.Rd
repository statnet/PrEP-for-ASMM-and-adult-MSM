% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod.initialize.camplc.R
\name{reinit_msm}
\alias{reinit_msm}
\title{Re-Initialization Module}
\usage{
reinit_msm(x, param, init, control, s)
}
\arguments{
\item{x}{An \code{EpiModel} object of class \code{\link{netsim}}.}
}
\value{
This function resets the data elements on the \code{dat} master data object
in the needed ways for the time loop to function.
}
\description{
This function reinitializes an epidemic model to restart at a
             specified time step given an input \code{netsim} object.
}
\keyword{module}
\keyword{msm}
