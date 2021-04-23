if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "sd_bias", "mean_bias", "group",
  "total_length", "value_length", "values"
))

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "package 'PAutilities' was built under R version 3.5.0"
  )

}
