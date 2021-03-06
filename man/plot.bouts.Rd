% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bout_general_4plot.R
\name{plot.bouts}
\alias{plot.bouts}
\title{Visualize the result of a bout analysis}
\usage{
\method{plot}{bouts}(x, ...)
}
\arguments{
\item{x}{the object to plot}

\item{...}{further methods passed to or from methods, currently unused}
}
\value{
A \code{ggplot2} object that visualizes the bout analysis
}
\description{
Visualize the result of a bout analysis
}
\examples{
data(ex_data, package = "PAutilities")
intensity <- as.character(get_intensity(ex_data$METs))
\donttest{
  bouts <- get_bouts(intensity, "cluster-based", "MVPA", 30, 5, 50, 3)
  plot(bouts)
}
}
