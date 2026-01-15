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
