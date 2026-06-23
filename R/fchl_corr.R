#' @title Temperature correction for (some) chlorophyll fluorometers
#'
#' @description Requires temperature (celsius) and raw chlorophyll fluorescence data (ug/L). Applies Watras (2017) correction using 25 degrees celcius as the reference temperature. This should be applied to YSI 6600, YSI EXO, and Seabird WETStar data.
#' 
#' @param raw_fchl Vector of numbers
#' @param temp_c Vector of numbers
#' @param instr One of <X, Y, Z>
#' @param na_rm Whether `NA`s should be removed. Defaults to `TRUE`
#'
#' @returns Corrected chlorophyll concentration data using 'temp_c' for the numbers provided to 'raw_fchl'
#' @export
#' 
#' @examples
#' # Temperature correction (for 6600, EXO2, or WS)
#' 

correct_fchl <- function(raw_fchl = NULL, temp_c = NULL,
  instr = "EXO2", bias_corr = NULL, na_rm = TRUE){

  # Error checks here

  # Warnings here


  # Actual correction code
  library(dplyr)
  if(instr == "EXO2"){
    corr.temp_fchl <- raw_fchl / (1 + (0.01 * (temp_c - 25)))
    corr.instr_fchl <- (1.29 * corr.temp_fchl) + 0.33
  } else if (instr == "FP"){
    corr.instr_fchl <- chl_raw %>% 
  mutate(chl_raw = case_when(
    chl_raw < 16 ~ 0.39*chl_raw+0.33,
    chl_raw >= 16 ~ 0.71*chl_raw-4.66
  ))  }else if (instr == "6600"){
  corr.temp_fchl <- raw_fchl / (1 + (0.01 * (temp_c - 25)))
  }else if (instr == "WS"){
    corr.temp_fchl <- raw_fchl / (1 + (0.01 * (temp_c - 25)))
        corr.instr_fchl <- (0.72 * corr.temp_fchl) - 0.06
  }
  # Return corrected fChl
  return(corr.instr_fchl)


}


