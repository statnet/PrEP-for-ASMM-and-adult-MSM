% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod.agemix.R
\name{agemix_campcl}
\alias{agemix_campcl}
\title{Track age mixing}
\usage{
agemix_campcl(dat, at)
}
\arguments{
\item{dat}{Master data list object of class \code{dat} containing networks,
individual-level attributes, and summary statistics.}

\item{at}{Current time step.}
}
\value{
This function returns the \code{dat} object with the updated edge
list (\code{el.age}). Each element of \code{el.age} is a data frame with the ids of
all active pairs with their respective ages.
}
\description{
Module function for tracking age on the edgelist.
}
\details{
The function is a diagnostic tool to determine the age mixing matrix for adult MSM and ASMM.
}
\keyword{module}
\keyword{msm}
