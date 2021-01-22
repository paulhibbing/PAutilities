## Note: This file generates internal package data, and so is not intended to be
## part of any processing pipeline or run repeatedly, etc.

rm(list = ls())
devtools::load_all()

equations <-
  readxl::read_excel("data-raw/0__Metabolic_Equations.xlsx") %>%
  data.frame(stringsAsFactors = FALSE)

source("data-raw/internal_add.R")
internal_add(equations)
