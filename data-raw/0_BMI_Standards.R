rm(list = ls())
load("R/sysdata.rda")
sys_list <- ls()
rm(standards)
source("data-raw/internal_add.R")

standards <- data.frame(
  data.table::fread(
    "data-raw/0__BMI_Standards.csv",
    stringsAsFactors = FALSE
  ),
  stringsAsFactors = FALSE
)

internal_add(standards)
