# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-correct_fchl.R"))

#working: 
#correct_fchl(fchl = 3.67, instr = "EXO2", skip_tcorr = FALSE, temp = 23.03, na.rm = TRUE)

# Error testing
test_that("Errors work as desired", {
  expect_error(correct_fchl(fchl = -2, instr = "EXO2", skip_tcorr = FALSE, temp = 23.03, na.rm = TRUE))
  expect_error(correct_fchl(fchl = "banana", instr = "EXO2", skip_tcorr = FALSE, temp = 23.03, na.rm = TRUE))
  expect_error(correct_fchl(fchl = NULL, instr = "EXO2", skip_tcorr = FALSE, temp = 23.03, na.rm = TRUE)) 
  
  expect_error(correct_fchl(fchl = 3.67, instr = NULL, skip_tcorr = FALSE, temp = 23.03, na.rm = TRUE))
  expect_error(correct_fchl(fchl = 3.67, instr = "banana", skip_tcorr = FALSE, temp = 23.03, na.rm = TRUE))

  expect_error(correct_fchl(fchl = 3.67, instr = "EXO2", skip_tcorr = FALSE, temp = NULL, na.rm = TRUE))
})

# Warning testing
test_that("Warnings work as desired", {
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", skip_tcorr = FALSE, temp = 100, na.rm = TRUE))  
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", skip_tcorr = FALSE, temp = 1, na.rm = TRUE))
  expect_warning(correct_fchl(fchl = 75, instr = "EXO2", skip_tcorr = FALSE, temp = 25, na.rm = TRUE))
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", skip_tcorr = FALSE, temp = 25, na.rm = "banana"))
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", skip_tcorr = "banana", temp = 25, na.rm = TRUE))
})

# Message testing
# test_that("Messages work as desired", {
#   # None in this function (yet)
#   expect_message()
# })

# Output testing
test_that("Outputs are as expected", {
  
  # Make a testing vector
  test_chl<- c(4, 5, 6, NA, 43)
  test_temp<- c(23, 24, NA, 25, NA)
  
  # Use the function to calculate CV and calculate it by hand
  fxn_correct_fchl <- correct_fchl(fchl = test_chl, instr = "WS", skip_tcorr = FALSE, temp = test_temp, na.rm = FALSE)

    test_corr.temp_fchl <- test_chl / (1 + (0.01 * (test_temp - 25)))
    test_correct_fchl  <- (0.72 * test_corr.temp_fchl) - 0.06
  

  # Check certain aspects of output
  expect_equal(fxn_correct_fchl, test_correct_fchl)
  expect_true(class(fxn_correct_fchl) == "numeric")
})
