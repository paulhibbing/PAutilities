% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bout_general_3undo.R
\name{bout_expand}
\alias{bout_expand}
\alias{bout_expand.default}
\title{Re-vectorize run length encoded data}
\usage{
bout_expand(bouts, ...)

\method{bout_expand}{default}(bouts, ...)
}
\arguments{
\item{bouts}{output from \code{\link{get_bouts}}}

\item{...}{arguments passed to methods}
}
\value{
a vector of individual values comprising the runs in \code{bouts}
}
\description{
Re-vectorize run length encoded data
}
\examples{
data(ex_data, package = "PAutilities")
intensity <- as.character(get_intensity(ex_data$METs))
\donttest{
bouts <- get_bouts(intensity, target = "MVPA", target_buffer = 1)
tail(bout_expand(bouts), 40)
}
}
