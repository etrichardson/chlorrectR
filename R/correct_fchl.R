#' @title chlorrectR: Temperature and bias corrections for a suite of chlorophyll fluorometers
#'
#' @description This package contains relevant correction formulae for several different types of chlorophyll fluorometers. Applies Watras (2017) correction using 25 degrees celcius as the reference temperature where relevant and Richardson (2025) bias corrections where relevant. This should be applied to YSI 6600, YSI EXO, and Seabird WETStar data.
#' 
#' @param fchl Vector of numbers.
#' @param instr Instrument type must be one of the following: "EXO2", "FP", "WS", "6600"
#' @param skip_tcorr Whether or not to skip the temperature correction step (for example, most USGS data already apply temperature corrections). Defaults to `FALSE`.
#' @param temp Vector of numbers (Temperature in Celsius)
#' @param na.rm Whether or not `NA`s should be removed. Defaults to `TRUE`.
#' 
#' @returns Corrected chlorophyll concentration data using 'temp' (if applicable) for the numbers provided to 'fchl'
#' @export
#' 
#' @importFrom magrittr %>%
#' 
#' @examples
#' # Example raw chlorophyll fluorescence and temperature vectors
#' #raw_chl<- c(4.56, 5.01, 6.21, NA, 43.95)
#' #temp_c <- c(23.0, 24.1, NA, 25.3, NA)
#' 
#' #test_df <- cbind(raw_chl, temp_c)
#' 
#' # Assuming that these data are produced by the YSI EXO2 and have not already been temperature corrected:
#' #correct_fchl(fchl = test_df$raw_chl, instr = "EXO2", skip_tcorr = FALSE, temp = test_df$temp_c, na.rm = TRUE)


correct_fchl <- function(fchl = NULL, instr = NULL, 
  skip_tcorr = FALSE, temp = NULL, na.rm = TRUE){

  # Error checks here

  if(is.null(fchl)==TRUE || is.numeric(fchl)!=TRUE || min(fchl, na.rm = TRUE) < 0)
    stop("fchl must be a vector of positive numbers")

  if(is.null(instr)==TRUE || is.character(instr)!=TRUE || instr %in% c("EXO2", "FP", "WS", "6600") != TRUE) 
    stop('Instrument type must be one of the following: "EXO2", "FP", "WS", "6600"')
  
  if(is.logical(na.rm)!=TRUE){
    warning("na.rm must be TRUE or FALSE. Setting to default (TRUE)")
    na.rm <- TRUE }

  if(instr != "FP"){
    if(is.null(temp)==TRUE || is.numeric(temp)!=TRUE || length(fchl)!=length(temp))
      stop("Temperature must be a vector of as many numbers as chlorophyll") 
  }
  
  # Warnings here

  if(is.logical(skip_tcorr)!=TRUE){
    warning("skip_tcorr must be TRUE or FALSE. Setting to default (FALSE)")
    skip_tcorr <- FALSE}

  if(min(temp, na.rm = TRUE)<=10 | max(temp, na.rm = TRUE)>=45)
    warning("Temperature must be in celsius")

  if(any(fchl[!is.na(fchl)] >50))
    warning("Chlorophyll fluorometer readings above 50 ug/L are less likely to be accurate")  
  
  # Actual correction code

if(skip_tcorr == FALSE & instr != "FP"){
  corr.temp_fchl <- fchl / (1 + (0.01 * (temp - 25)))}
  else {corr.temp_fchl <- fchl}

  if(instr == "EXO2"){
    corr.instr_fchl <- (1.29 * corr.temp_fchl) + 0.33
  } 
  else if (instr == "FP"){
    corr.instr_fchl <- fchl %>% 
    dplyr::mutate(fchl = dplyr::case_when(
    fchl < 16 ~ 0.39*fchl+0.33,
    fchl >= 16 ~ 0.71*fchl-4.66))  
  }
  else if (instr == "6600"){
    corr.instr_fchl <- corr.temp_fchl
  }
  else if (instr == "WS"){
    corr.instr_fchl <- (0.72 * corr.temp_fchl) - 0.06
  }

  if(na.rm == TRUE) {

    final_fchl <- corr.instr_fchl[!is.na(corr.instr_fchl)]
  } else {final_fchl <- corr.instr_fchl}

  # Return corrected fChl
  return(final_fchl)
}
