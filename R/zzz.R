if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "CI_high", "CI_low", "CI_sig", "group", "high",
  "length_total", "length_value", "low", "mean_bias",
  "sd_bias", "values", "vartype", "x_label", "y"
))

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "package 'PAutilities' was built under R version 4.2.2"
  )

}
