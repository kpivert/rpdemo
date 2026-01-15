test_that("suppress_counts", {
  # Correct usage
  expect_silent(res <- suppress_counts(c(1, 3, 5, 7, 9)))
  expect_equal(res, c(NA, NA, 5, 7, 9))

  expect_silent(res <- suppress_counts(c(10, 20, 3, 4, 5), threshold = 10))
  expect_true(all(is.na(res[3:5])))

  expect_silent(res <- suppress_counts(1:10, threshold = 5, symbol = "*"))
  expect_true(all(res[1:4] == "*"))

  # Edge cases
  expect_silent(res <- suppress_counts(numeric(0)))
  expect_equal(length(res), 0)

  expect_silent(res <- suppress_counts(c(4, 4, 4), threshold = 5))
  expect_equal(res, c(NA_real_, NA_real_, NA_real_))

  # Incorrect usage
  expect_error(suppress_counts("not numeric"), "Input x must be numeric")
  expect_error(suppress_counts(c("a", "b", "c")), "Input x must be numeric")
})

test_that("standardize_sex", {
  # Standard inputs
  expect_equal(standardize_sex("Male"), "M")
  expect_equal(standardize_sex("male"), "M")
  expect_equal(standardize_sex("M"), "M")
  expect_equal(standardize_sex("m"), "M")

  expect_equal(standardize_sex("Female"), "F")
  expect_equal(standardize_sex("female"), "F")
  expect_equal(standardize_sex("F"), "F")
  expect_equal(standardize_sex("f"), "F")

  # Other gender identities
  expect_equal(standardize_sex("Trans female"), "O")
  expect_equal(standardize_sex("trans male"), "O")
  expect_equal(standardize_sex("transgender"), "O")
  expect_equal(standardize_sex("nonbinary"), "O")
  expect_equal(standardize_sex("non-binary"), "O")
  expect_equal(standardize_sex("binary"), "O")

  # Unknown/other values
  expect_equal(standardize_sex("unknown"), "U")
  expect_equal(standardize_sex("other"), "U")
  expect_equal(standardize_sex("xyz"), "U")
  expect_equal(standardize_sex(NA_character_), "U")

  # Whitespace handling
  expect_equal(standardize_sex("  Male  "), "M")
  expect_equal(standardize_sex(" female "), "F")

  # Vector inputs
  result <- standardize_sex(c(
    "Male",
    "f",
    "Trans female",
    "nonbinary",
    "unknown",
    NA
  ))
  expect_equal(result, c("M", "F", "O", "O", "U", "U"))

  # Error handling
  expect_error(standardize_sex(123), "Input x must be a character vector")
  expect_error(
    standardize_sex(c(1, 2, 3)),
    "Input x must be a character vector"
  )
})

test_that("read_labdata", {
  # Read the example file
  fp <- system.file("extdata/rped.csv", package = "rpdemo", mustWork = TRUE)

  expect_silent(df <- read_labdata(fp))

  # Check structure
  expect_s3_class(df, "data.frame")
  expect_true("sex" %in% names(df))
  expect_true("age" %in% names(df))
  expect_true("zip" %in% names(df))

  # Check column types
  expect_type(df$sex, "character")
  expect_type(df$age, "integer")
  expect_type(df$zip, "character")
})

test_that("prep_lab_data with warnings", {
  # Test with default warnings = TRUE

  # Create test data with various issues
  test_data <- data.frame(
    id = 1:5,
    sex = c("Male", "female", "M", "nonbinary", "unknown"),
    age = c(25, 30, 15, 50, -5), # includes minor and unrealistic age
    zip = c("22201", "10001", "99999", "20001", "00000"), # includes bad zip
    lab1 = c(1, 2, 3, 4, 5),
    lab2 = c(10, 20, 30, 40, 50)
  )

  # Expect multiple warnings
  expect_warning(
    expect_warning(
      expect_warning(
        result <- prep_lab_data(test_data),
        "bad zip code"
      ),
      "minor"
    ),
    "unrealistic age"
  )

  # Check that sex is standardized
  expect_true(all(result$sex %in% c("M", "F", "O", "U")))

  # Check that zipcodes are joined
  expect_true("city" %in% names(result) | "state" %in% names(result))

  # Check that lab values are scaled
  expect_true(all(c("lab1", "lab2") %in% names(result)))
})

test_that("prep_lab_data without warnings", {
  # Test with warnings = FALSE
  test_data <- data.frame(
    id = 1:3,
    sex = c("Male", "female", "M"),
    age = c(25, 30, 10), # includes minor
    zip = c("22201", "10001", "99999"), # includes bad zip
    lab1 = c(1, 2, 3),
    lab2 = c(10, 20, 30)
  )

  # Should not produce warnings when warnings = FALSE
  expect_silent(result <- prep_lab_data(test_data, warnings = FALSE))

  # Check result structure
  expect_s3_class(result, "data.frame")
  expect_true(all(result$sex %in% c("M", "F", "O", "U")))
})

test_that("prep_lab_data with clean data", {
  # Test with data that shouldn't trigger warnings
  test_data <- data.frame(
    id = 1:3,
    sex = c("Male", "female", "M"),
    age = c(25, 30, 45),
    zip = c("22201", "10001", "20001"), # all valid zips
    lab1 = c(1, 2, 3),
    lab2 = c(10, 20, 30)
  )

  expect_silent(result <- prep_lab_data(test_data, warnings = TRUE))

  # Verify transformations
  expect_equal(nrow(result), nrow(test_data))
  expect_true(all(result$sex %in% c("M", "F", "O", "U")))
})

test_that("prep_lab_data edge cases", {
  # Test with actual package data
  expect_silent(result <- prep_lab_data(rped, warnings = FALSE))
  expect_s3_class(result, "data.frame")

  # Verify scaled lab values have mean ~0 and sd ~1
  if ("lab1" %in% names(result)) {
    expect_true(abs(mean(result$lab1, na.rm = TRUE)) < 0.01)
    expect_true(abs(sd(result$lab1, na.rm = TRUE) - 1) < 0.01)
  }
})
