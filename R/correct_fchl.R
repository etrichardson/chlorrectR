#' @title chlorrectR: Temperature and bias corrections for a suite of chlorophyll fluorometers
#'
#' @description This package contains relevant correction formulae for several different types of chlorophyll fluorometers. Applies Watras (2017) correction using 25 degrees celcius as the reference temperature where relevant and Richardson (2025) bias corrections where relevant. This should be applied to YSI 6600, YSI EXO, and Seabird WETStar data.
#' 
#' @param fchl Vector of numbers.
#' @param instr Instrument type must be one of the following: "EXO2", "FP", "WS", "6600"
#' @param skip_tcorr Whether or not to skip the temperature correction step (for example, most USGS data already apply temperature corrections). Defaults to `FALSE`.
#' @param temp Vector of numbers (Temperature in Celsius)
#' 
#' @returns Corrected chlorophyll concentration data using 'temp' (if applicable) for the numbers provided to 'fchl'
#' @export
#' 
#' @importFrom magrittr %>%
#' 
#' @examples
#' # library(dplyr)
#' # Example raw chlorophyll fluorescence and temperature vectors
#' # raw_chl<- c(4.56, 5.01, 6.21, NA, 43.95)
#' # temp_c <- c(23.0, 24.1, NA, 25.3, NA)
#' 
#' # test_df <- cbind(raw_chl, temp_c)
#' 
#' # Assuming that these data are produced by the YSI EXO2 and have not already been temperature corrected:
#' # test_df <- test_df %>% mutate(corr_chl = correct_fchl(fchl = raw_chl, instr = "EXO2", skip_tcorr = FALSE, temp = temp_c))
#' 
#' # Returns the following dataframe:
#' 
#' #> test_df 
#' #  raw_chl temp_c corr_chl
#' #1    4.56   23.0     6.33
#' #2    5.01   24.1     6.85
#' #3    6.21     NA       NA
#' #4      NA   25.3       NA
#' #5   43.95     NA       NA



correct_fchl <- function(fchl = NULL,
                         instr = c("EXO2", "FP", "WS", "6600"),
                         skip_tcorr = FALSE,
                         temp = NULL) {

  ## ---------------------------
  ## Validate fchl
  ## ---------------------------
  if (is.null(fchl) || !is.numeric(fchl) || any(fchl < 0, na.rm = TRUE)) {
    stop("fchl must be a vector of positive numbers")
  }

  ## ---------------------------
  ## Validate instrument
  ## ---------------------------
  instr <- match.arg(instr)

  ## ---------------------------
  ## Validate skip_tcorr
  ## ---------------------------
  if (!is.logical(skip_tcorr) || length(skip_tcorr) != 1) {
    warning("skip_tcorr must be TRUE or FALSE. Setting to FALSE")
    skip_tcorr <- FALSE
  }

  ## ---------------------------
  ## Decide if temperature is needed
  ## ---------------------------
  needs_temp <- !skip_tcorr && instr != "FP"

  ## ---------------------------
  ## Validate temp only if needed
  ## ---------------------------
  if (needs_temp) {

    if (is.null(temp) || !is.numeric(temp) || length(temp) != length(fchl)) {
      stop("Temperature must be a numeric vector with the same length as fchl")
    }

    if (any(temp < 10, na.rm = TRUE) || any(temp > 45, na.rm = TRUE)) {
      warning("Temperature appears to be outside expected Celsius range")
    }
  }

  ## ---------------------------
  ## Warnings unrelated to temp requirement
  ## ---------------------------
  if (any(fchl > 50, na.rm = TRUE)) {
    warning("Chlorophyll fluorometer readings above 50 ug/L are less likely to be accurate")
  }

  ## ---------------------------
  ## Temperature correction
  ## ---------------------------
  if (needs_temp) {
    corr.temp_fchl <- fchl / (1 + (0.01 * (temp - 25)))
  } else {
    corr.temp_fchl <- fchl
  }

  ## ---------------------------
  ## Instrument correction
  ## ---------------------------
  corr.instr_fchl <- switch(
    instr,

    "EXO2" = (1.29 * corr.temp_fchl) + 0.33,

    "FP" = ifelse(
      fchl < 16,
      0.39 * fchl + 0.33,
      0.71 * fchl - 4.66
    ),

    "6600" = corr.temp_fchl,

    "WS" = (0.72 * corr.temp_fchl) - 0.06
  )

  ## ---------------------------
  ## Return
  ## ---------------------------
  return(round(corr.instr_fchl, 2))
}
