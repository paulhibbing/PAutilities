% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weight_status.R
\name{weight_status}
\alias{weight_status}
\title{Determine weight status from body mass index}
\usage{
weight_status(BMI = NULL, breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
  labels = c("Underweight", "Healthy Weight", "Overweight", "Class 1 Obese",
  "Class 2 Obese", "Class 3 Obese"), right = FALSE, youth = FALSE, ...)

#get_BMI_percentile(weight_kg, height_cm, age_yrs = NULL, age_mos = NULL,
  #sex = c("Male", "Female"), BMI = NULL, df = NULL,
  #output = c("percentile", "classification", "both", "summary"))
}
\arguments{
\item{BMI}{numeric. The participant body mass index}

\item{breaks}{numeric vector. The boundaries for each weight class; passed to
\code{base::cut}, with warnings if \code{-Inf} and \code{Inf} are not
included in the vector.}

\item{labels}{character vector. The labels for each weight class; passed to
\code{base::cut}, and should have a length one less than the length of
\code{breaks}}

\item{right}{logical. See \code{?base::cut}}

\item{youth}{logical. Use function as a wrapper for
\code{\link{get_BMI_percentile}}?}

\item{...}{Arguments passed to \code{\link{get_BMI_percentile}}}
}
\value{
a factor reflecting weight status
}
\description{
Allows users to determine weight status from body mass index (BMI). The
function is designed to classify adult weight status, with default settings
yielding weight classes defined by the Centers for Disease Control and
Prevention (see reference below). Alternatively, the function can be used as
a wrapper for \code{\link{get_BMI_percentile}} to obtain classifications for
youth.
}
\examples{
weight_status(17:42)
}
\references{
\url{https://www.cdc.gov/bmi/adult-calculator/bmi-categories.html}
}
