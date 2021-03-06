% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmmaExampleData.R
\docType{data}
\name{hmmaExampleData}
\alias{hmmaExampleData}
\title{An example dataset for use on HMM-As}
\format{
A list with the following items
\describe{
  \item{x}{All observations.}
  \item{N}{The length of the observation sequence.}
}
}
\usage{
hmmaExampleData
}
\description{
This dataset has been generated using a HMM-A with two different Bayesian
networks. The observations consist of 100 sequences of length 20.
}
\details{
The sum of all observation lengts (hmmaExampleData$N) must be equal to the
total number of observations (hmmExampleData$x).
}
\keyword{datasets}
