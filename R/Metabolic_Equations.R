#' Retrieve Schofield basal metabolic rate for an individual
#'
#' @param Sex The participant's sex, either \code{M} or \code{F}
#' @param Ht The participant's height, in meters
#' @param Wt The participant's weight, in kilograms
#' @param Age The participant's age, in years
#' @param verbose Logical. Should processing updates be printed?
#' @param equation The equation to apply
#' @param MJ_conversion The value to use for kcals per megajoule
#' @param kcal_conversion The value to use for kcals per L of oxygen consumption
#'
#' @export
#'
get_schofield <-
  function(Sex, Ht = NULL, Wt, Age,
           verbose = TRUE, equation = c("both", "ht_wt", "wt"),
           MJ_conversion = 239.006, kcal_conversion = 4.86){

  # Sex <- "M"
  # Ht <- 173
  # Wt <- 55
  # Age <- 29
  # verbose <- TRUE
  # equation <- "both"

  ## Get set up
    if (verbose) cat('\nCalculating Schofield predicted",
          "basal metabolic rate.\n')

    if (!is.null(Ht)) {
      if (Ht > 3){
        message("Detected height in cm. Converting to M.")
        Ht <- Ht / 100
      }
    }

    equation <- match.arg(equation)
    if (equation == "both") equation <- c("wt", "ht_wt")

  ## Match participant to proper equation

    agegroup <- as.character(cut(
      Age,
      breaks = c(-Inf, 3, 10, 18, 30, 60, Inf),
      right = FALSE
    ))
    agegroup <-
      gsub(',', 'to', gsub("[\\[)]", "", agegroup))

    Sex <- c("male", "female")[pmatch(tolower(Sex), c("male", "female"))]

    weights <- lapply(schofield_weights, function(x) {
      x[grepl(paste('', Sex, agegroup, sep = '_'), rownames(x)), ]
    })

  ## Perform calculations

    ht_wt <- data.frame()
    wt_only <- data.frame()

    # ht_wt equation
    if ("ht_wt" %in% equation) {

      schofield_basal_mj <-
        (weights$weight_height$weight * Wt) +
        (weights$weight_height$height * Ht) +
        (weights$weight_height$intercept)

      schofield_basal_VO2_mlkgmin <-
        schofield_basal_mj * MJ_conversion / 24 / 60 /
        kcal_conversion / Wt * 1000

      ht_wt <- data.frame(
        equation = "Weight_and_Height",
        MJ_conversion = MJ_conversion,
        kcal_conversion = kcal_conversion,
        schofield_basal_mj = schofield_basal_mj,
        schofield_basal_VO2_mlkgmin = schofield_basal_VO2_mlkgmin
      )

    } # End ht_wt equation

    ###

    # wt equation
    if ("wt" %in% equation) {

      schofield_basal_mj <-
        (weights$weight_only$weight * Wt) +
        (weights$weight_only$intercept)

      schofield_basal_VO2_mlkgmin <-
        schofield_basal_mj * MJ_conversion / 24 / 60 /
        kcal_conversion / Wt * 1000

      wt_only <- data.frame(
        equation = "Weight_Only",
        MJ_conversion = MJ_conversion,
        kcal_conversion = kcal_conversion,
        schofield_basal_mj = schofield_basal_mj,
        schofield_basal_VO2_mlkgmin = schofield_basal_VO2_mlkgmin
      )
    } # End wt equation

  return(
    rbind(ht_wt, wt_only)
  )
}




#' Calculate energy expenditure using the Weir equation
#'
#' @param VO2 Oxygen consumption
#' @param VCO2 Carbon dioxide production
#' @param epochSecs The averaging window of the metabolic data, in seconds
#'
#' @export
#'
weir_equation <- function(VO2, VCO2, epochSecs){
  kcal <- 1.44*(3.94*VO2 + 1.11*VCO2)
  kcal <- (kcal / (24*60*60))*epochSecs
  return(kcal)
}
