# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-correct_fchl.R"))


#working: 
correct_fchl(fchl = 3.67, instr = EXO2, temp = 23.03, na_rm = TRUE)


# Error testing
test_that("Errors work as desired", {
  expect_error(correct_fchl(fchl = -2, instr = EXO2, temp = 23.03, na_rm = TRUE))
  expect_error(correct_fchl(fchl = "banana", instr = EXO2, temp = 23.03, na_rm = TRUE))
  expect_error(correct_fchl(fchl = NULL, instr = EXO2, temp = 23.03, na_rm = TRUE)) 
  
  expect_error(correct_fchl(fchl = 3.67, instr = NULL, temp = 23.03, na_rm = TRUE))
  expect_error(correct_fchl(fchl = 3.67, instr = EXO, temp = 23.03, na_rm = TRUE))


  expect_error(correct_fchl(fchl = 3.67, instr = EXO2, temp = NULL, na_rm = TRUE))

  expect_error(correct_fchl(fchl = 3.67, instr = EXO2, temp = 23.03, na_rm = banana))
})

# Warning testing
test_that("Warnings work as desired", {
  expect_error(correct_fchl(fchl = 3.67, instr = EXO2, temp = 100, na_rm = TRUE))  
  expect_error(correct_fchl(fchl = 3.67, instr = EXO2, temp = 1, na_rm = TRUE))
  expect_error(correct_fchl(fchl = 75, instr = EXO2, temp = 25, na_rm = TRUE))
})

# Message testing
# test_that("Messages work as desired", {
#   # None in this function (yet)
#   expect_message()
# })

# Output testing
test_that("Outputs are as expected", {
  
  # Make a testing vector
  test_vec <- c(4, 5, 6)
  
  # Use the function to calculate CV and calculate it by hand
  fxn_correct_fchl <- correct_fchl(x = test_vec, na_rm = FALSE)
  test_sd <- sd(x = test_vec, na.rm = FALSE)
  test_avg <- mean(x = test_vec, na.rm = FALSE)
  test_cv <- (test_sd / test_avg)

  # Check certain aspects of output
  expect_equal(fxn_correct_fchl, test_correct_fchl)
  expect_true(class(fxn_correct_fchl) == "numeric")
})
