#' Statistics for numeric summaries
#' 
#' @description
#' Functions for computing summary statistics for use in `cb_summarize_numeric()`.
#' - `skew()` and `kurtosis()` return adjusted skewness and kurtosis. (Unadjusted 
#'   estimates can be obtained by setting `adjusted = FALSE`.)
#' - `spread()` returns the difference between a vector's minimum and maximum values.
#' - `min_if_any()` and `max_if_any()` return minima and maxima with alternate behavior 
#'   if all values are missing. (Re-exported from the lighthouse package. See `lighthouse::min_if_any` 
#'   for more details.)
#' - `se_mean()` returns the standard error of the mean. (Re-exported from the lighthouse 
#'   package. See `lighthouse::se_mean` for more details.)
#' 
#' @param x A numeric vector.
#' 
#' @param ... A numeric vector or vectors.
#' 
#' @param na.rm Should missing values be removed? (Note that `cb_summarize_numeric()` 
#' automatically sets na.rm to `TRUE` for all functions).
#' 
#' @param adjusted If `TRUE`, returns adjusted skewness (_G_<sub>1</sub>) or kurtosis 
#' (_G_<sub>2</sub>) by applying a small-sample correction. If `FALSE`, returns 
#' the unadjusted skewness (_g_<sub>1</sub>) or kurtosis (_g_<sub>2</sub>). (Note 
#' that `TRUE` corresponds to behavior of software such as SPSS, SAS, and Excel.)
#' 
#' @param excess If `TRUE`, returns excess kurtosis by (total kurtosis - 3).
#' 
#' @name stats
#' 
#' @rdname stats
#' @export
skew <- function(x, adjusted = TRUE, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < 3) return(NA_real_)
  
  m  <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  
  if (m2 == 0) return(0)
  
  g1 <- m3 / (m2^(3/2))
  
  if (!adjusted) return(g1)
  
  sqrt(n * (n - 1)) / (n - 2) * g1
}
#' 
#' @rdname stats
#' @export
kurtosis <- function(x, adjusted = TRUE, excess = TRUE, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)

  m  <- mean(x)
  m2 <- mean((x - m)^2)
  m4 <- mean((x - m)^4)

  if (m2 == 0) return(if (excess) 0 else 3)

  g2 <- m4 / (m2^2)
  g2_excess <- g2 - 3

  if (!adjusted) return(if (excess) g2_excess else g2)

  G2_excess <- ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * g2_excess + 6)

  if (excess) G2_excess else G2_excess + 3
}
#' 
#' @rdname stats
#' @export
spread <- function(x, na.rm = FALSE) {
  max_if_any(x, na.rm = na.rm) - min_if_any(x, na.rm = na.rm)
}
#' 
#' @importFrom lighthouse min_if_any max_if_any se_mean
#' @rdname stats
#' @export
lighthouse::min_if_any
#' 
#' @rdname stats
#' @export
lighthouse::max_if_any
#' 
#' @rdname stats
#' @export
lighthouse::se_mean
