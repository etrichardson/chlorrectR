# Run all tests in this script:
testthat::test_file(file.path("tests", "testthat", "test_correct_fchl.R"))

# Error testing
test_that("Errors work as desired", {
   expect_error(correct_fchl(x = NULL, na_rm = TRUE))
   expect_error(correct_fchl(x = "not a number", na_rm = TRUE))
 })

# Warning testing
test_that("Warnings work as desired", {
   expect_warning(correct_fchl(x = c(1, 2, 3), na_rm = "false"))
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
