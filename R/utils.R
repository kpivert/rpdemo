#' Suppress Low Counts
#'
#' This suppressed values with low count for privacy reasons.
#'
#' @param x a numeric vector of counts
#' @param threshold numeric thresholds for the number below which to suppress
#' @param symbol What the suppressed values should be, default is NA
#' @returns A vector with low counts suppressed
#'
#' @export
#' @examples
#' suppress_counts(1:10)
#' suppress_counts(1:20, threshold= 20)
#' suppress_counts(1:20, symbol = "*")
suppress_counts <- function(x, threshold = 5, symbol = NA) {
  if (!is.numeric(x)) {
    stop("Input x must be numeric.")
  }
  x[x < threshold] <- symbol
  return(x)
}

#' Standardize Sex Variables
#'
#' Maps various inputs (M, Male, f, Female) to standard HL7/ISO codes (M, F, O, U).
#'
#' @param x A character vector of sex/gender inputs.
#'
#' @return A character vector containing "M", "F", "O", or "U".
#' @export
#'
#' @examples
#' standardize_sex(c("Male", "f", "Trans female", "nonbinary", "unknown", NA))
standardize_sex <- function(x) {
  # Check if input is character
  if (!is.character(x)) {
    stop("Input x must be a character vector.")
  }

  # trim whitespace and make lowercase
  x <- trimws(tolower(as.character(x)))

  # Map various inputs to standard codes
  standardized <- dplyr::case_when(
    x %in% c("male", "m") ~ "M",
    x %in% c("female", "f") ~ "F",
    grepl("trans|non|binary", x) ~ "O",
    .default = "U"
  )

  return(standardized)
}
