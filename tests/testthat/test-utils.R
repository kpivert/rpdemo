test_that("suppress_counts", {
  # Correct usage
  expect_silent(res <- suppress_counts(c(1, 3, 5, 7, 9)))
  expect_equal(res, c(NA, NA, 5, 7, 9))
  expect_silent(res <- suppress_counts(c(10, 20, 3, 4, 5), threshold = 10))
  expect_true(all(is.na(res[3:5])))
  expect_silent(res <- suppress_counts(1:10, threshold = 5, symbol = "*"))
  expect_true(all(res[1:4] == "*"))
  # Incorrect usage
  expect_error(suppress_counts("not numeric"))
})
