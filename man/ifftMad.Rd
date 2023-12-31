% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fftMad.R
\name{ifftMad}
\alias{ifftMad}
\title{Calculate the inverse fast fourier transform}
\usage{
ifftMad(x)
}
\arguments{
\item{x}{vector given to have inverse fast fourier transform}
}
\value{
inverse fast fourier transform of x
}
\description{
TRhis function calculate the inverse fast fourier transform of a given vector
}
\examples{
ifftMad(seq(0.2*pi, length.out = 20))
}
