% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unscale.R
\name{unscale}
\alias{unscale}
\title{Reverse scaling and centering of a vector}
\usage{
unscale(x)
}
\arguments{
\item{x}{A vector that has been scaled and centered}
}
\value{
The original vector before scaling and centering
}
\description{
Reverse scaling and centering of a vector
}
\examples{
x = scale(1:10)
unscale(x)

}