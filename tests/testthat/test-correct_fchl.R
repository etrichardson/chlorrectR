# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-correct_fchl.R"))


#working: 
correct_fchl(fchl = 3.67, instr = "EXO2", temp = 23.03, na_rm = TRUE)


# Error testing
test_that("Errors work as desired", {
  expect_error(correct_fchl(fchl = -2, instr = "EXO2", temp = 23.03, na_rm = TRUE))
  expect_error(correct_fchl(fchl = "banana", instr = "EXO2", temp = 23.03, na_rm = TRUE))
  expect_error(correct_fchl(fchl = NULL, instr = "EXO2", temp = 23.03, na_rm = TRUE)) 
  
  expect_error(correct_fchl(fchl = 3.67, instr = NULL, temp = 23.03, na_rm = TRUE))
  expect_error(correct_fchl(fchl = 3.67, instr = "banana", temp = 23.03, na_rm = TRUE))

  expect_error(correct_fchl(fchl = 3.67, instr = "EXO2", temp = NULL, na_rm = TRUE))

})

# Warning testing
test_that("Warnings work as desired", {
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", temp = 100, na_rm = TRUE))  
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", temp = 1, na_rm = TRUE))
  expect_warning(correct_fchl(fchl = 75, instr = "EXO2", temp = 25, na_rm = TRUE))
  expect_warning(correct_fchl(fchl = 3.67, instr = "EXO2", temp = 23.03, na_rm = "banana"))
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
  fxn_correct_fchl <- correct_fchl(fchl = test_chl, instr = "WS", temp = test_temp, na_rm = FALSE)

    test_corr.temp_fchl <- test_chl / (1 + (0.01 * (test_temp - 25)))
    test_correct_fchl  <- (0.72 * test_corr.temp_fchl) - 0.06
  

  # Check certain aspects of output
  expect_equal(fxn_correct_fchl, test_correct_fchl)
  expect_true(class(fxn_correct_fchl) == "numeric")
})
