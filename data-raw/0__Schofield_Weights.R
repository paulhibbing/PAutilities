## Note: This file generates internal package data, and so is not intended to be
## part of any processing pipeline or run repeatedly, etc.

rm(list = ls())
devtools::load_all()

weights_wt_ht <- do.call(rbind, list(
  schofield_male_less3    = data.frame(weight = .0007,
                                       height = 6.349,
                                       intercept = -2.584),
  schofield_female_less3  = data.frame(weight = .068,
                                       height = 4.281,
                                       intercept = -1.730),

  schofield_male_3to10    = data.frame(weight = .082,
                                       height = .545,
                                       intercept = 1.736),
  schofield_female_3to10  = data.frame(weight = .071,
                                       height = .677,
                                       intercept = 1.553),

  schofield_male_10to18   = data.frame(weight = .068,
                                       height = .574,
                                       intercept = 2.157),
  schofield_female_10to18 = data.frame(weight = .035,
                                       height = 1.948,
                                       intercept = .837),

  schofield_male_18to30   = data.frame(weight = .063,
                                       height = -.042,
                                       intercept = 2.953),
  schofield_female_18to30 = data.frame(weight = .057,
                                       height = 1.184,
                                       intercept = .411),

  schofield_male_30to60   = data.frame(weight = .048,
                                       height = -.011,
                                       intercept = 3.670),
  schofield_female_30to60 = data.frame(weight = .034,
                                       height = .006,
                                       intercept = 3.530),

  schofield_male_over60   = data.frame(weight = .038,
                                       height = 4.068,
                                       intercept = -3.491),
  schofield_female_over60 = data.frame(weight = .033,
                                       height = 1.917,
                                       intercept = 0.074))
)

weights_wt <- do.call(rbind, list(
  schofield_male_less3    = data.frame(weight = 0.249,
                                       intercept = -0.127),
  schofield_female_less3  = data.frame(weight = 0.244,
                                       intercept = -0.130),

  schofield_male_3to10    = data.frame(weight = 0.095,
                                       intercept = 2.110),
  schofield_female_3to10  = data.frame(weight = 0.085,
                                       intercept = 2.033),

  schofield_male_10to18   = data.frame(weight = 0.074,
                                       intercept = 2.754),
  schofield_female_10to18 = data.frame(weight = 0.056,
                                       intercept = 2.898),

  schofield_male_18to30   = data.frame(weight = 0.063,
                                       intercept = 2.896),
  schofield_female_18to30 = data.frame(weight = 0.062,
                                       intercept = 2.036),

  schofield_male_30to60   = data.frame(weight = 0.048,
                                       intercept = 3.653),
  schofield_female_30to60 = data.frame(weight = 0.034,
                                       intercept = 3.538),

  schofield_male_over60   = data.frame(weight = 0.049,
                                       intercept = 2.459),
  schofield_female_over60 = data.frame(weight = 0.038,
                                       intercept = 2.755))
)

schofield_weights <- list(
  weight_only = weights_wt,
  weight_height = weights_wt_ht
)

source("data-raw/internal_add.R")
internal_add(schofield_weights)

# Add FAO Schofield object ------------------------------------------------

rm(list = ls())
fao <- data.frame(readxl::read_excel(
  "data-raw/0__FAO_Schofield.xlsx"
))

row.names(fao) <- fao$group
fao$group <- NULL

source("data-raw/internal_add.R")
internal_add(fao)
