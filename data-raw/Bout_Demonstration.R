rm(list = ls())
devtools::load_all()

ex_data <-
  data.frame(data.table::fread("data-raw/ex_data.csv"))

devtools::use_data(ex_data, overwrite = TRUE)
