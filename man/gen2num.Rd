% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen2num.R
\name{gen2num}
\alias{gen2num}
\title{gen2num}
\usage{
gen2num(g)
}
\arguments{
\item{g}{A string or string vector.}
}
\value{
A numeric or numeric vector holding the numbers corresponding to the
generators in `g`
}
\description{
`gen2num` converts a generator into its corresponding column
number.
}
\examples{
gen2num("abc")
gen2num(g = c("acd", "e", "cdf"))
}
