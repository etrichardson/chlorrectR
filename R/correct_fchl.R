#' @title Temperature and bias correction for chlorophyll fluorometers
#'
#' @description Requires temperature (celsius) and raw chlorophyll fluorescence data (ug/L). Applies Watras (2017) correction using 25 degrees celcius as the reference temperature and Richardson (2025) bias corrections. This should be applied to YSI 6600, YSI EXO, and Seabird WETStar data.
#' 
#' @param fchl Vector of numbers
#' @param temp Vector of numbers
#' @param instr Instrument type must be one of the following: "EXO2", "FP", "WS", "6600"
#' @param na_rm Whether `NA`s should be removed. Defaults to `TRUE`
#'
#' @returns Corrected chlorophyll concentration data using 'temp' for the numbers provided to 'fchl'
#' @export
#' 
#' @examples
#' # Temperature correction (for 6600, EXO2, or WS)
#' 

correct_fchl <- function(fchl = NULL, 
  instr = NULL, temp = NULL, na_rm = TRUE){

  # Error checks here

if(is.null(fchl)==TRUE || is.numeric(fchl)!=TRUE || min(fchl, na.rm = TRUE) < 0)
    stop("fchl must be a vector of positive numbers")

if(is.null(instr)==TRUE || is.character(instr)!=TRUE || !instr %in% c("EXO2", "FP", "WS", "6600")) 
    stop('Instrument type must be one of the following: "EXO2", "FP", "WS", "6600"')
  
if(is.logical(na_rm)!=TRUE){
  warning("na_rm must be TRUE or FALSE. Setting to default (TRUE)")
  na_rm <- TRUE }

# if(na_rm == TRUE) {
#   chlv2 <- fchl[!is.na(fchl)]
#   tempv2 <- temp[!is.na(temp)]
# } else {
  chlv2 <- fchl
   tempv2 <- temp
# }

if(instr!= "FP"){
    if(is.null(tempv2)==TRUE || is.numeric(tempv2)!=TRUE || length(chlv2)!=length(tempv2))
    stop("Temperature must be a vector of as many numbers as chlorophyll") }
  
  # Warnings here

if(min(tempv2, na.rm = TRUE)<=10 | max(tempv2, na.rm = TRUE)>=45)
  warning("Temperature must be in celsius")

if(any(chlv2[!is.na(chlv2)] >50))
  warning("Chlorophyll fluorometer readings above 50 ug/L are less likely to be accurate")  
  
  # Actual correction code
  if(instr == "EXO2"){
    corr.temp_fchl <- chlv2 / (1 + (0.01 * (tempv2 - 25)))
    corr.instr_fchl <- (1.29 * corr.temp_fchl) + 0.33
  } 
  else if (instr == "FP"){
    corr.instr_fchl <- chlv2 %>% 
    dplyr::mutate(chlv2 = dplyr::case_when(
    chlv2 < 16 ~ 0.39*chlv2+0.33,
    chlv2 >= 16 ~ 0.71*chlv2-4.66))  
  }
  else if (instr == "6600"){
    corr.temp_fchl <- chlv2 / (1 + (0.01 * (tempv2 - 25)))
  }
  else if (instr == "WS"){
    corr.temp_fchl <- chlv2 / (1 + (0.01 * (tempv2 - 25)))
    corr.instr_fchl <- (0.72 * corr.temp_fchl) - 0.06
  }

  if(na_rm == TRUE) {

    final_fchl <- corr.instr_fchl[!is.na(corr.instr_fchl)]
  } else {final_fchl <- corr.instr_fchl}

  # Return corrected fChl
  return(final_fchl)
}
