# Statistics for numeric summaries

Functions for computing summary statistics for use in
[`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md).

- `skew()` and `kurtosis()` return adjusted skewness and kurtosis.
  (Unadjusted estimates can be obtained by setting `adjusted = FALSE`.)

- `spread()` returns the difference between a vector's minimum and
  maximum values.

- `min_if_any()` and `max_if_any()` return minima and maxima with
  alternate behavior if all values are missing. (Re-exported from the
  lighthouse package. See
  [`lighthouse::min_if_any`](https://ccsarapas.github.io/lighthouse/reference/aggregate_if_any.html)
  for more details.)

- `se_mean()` returns the standard error of the mean. (Re-exported from
  the lighthouse package. See
  [`lighthouse::se_mean`](https://ccsarapas.github.io/lighthouse/reference/se_mean.html)
  for more details.)

## Usage

``` r
skew(x, adjusted = TRUE, na.rm = FALSE)

kurtosis(x, adjusted = TRUE, excess = TRUE, na.rm = FALSE)

spread(x, na.rm = FALSE)

min_if_any(..., na.rm = TRUE)

max_if_any(..., na.rm = TRUE)

se_mean(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- adjusted:

  If `TRUE`, returns adjusted skewness (*G*₁) or kurtosis (*G*₂) by
  applying a small-sample correction. If `FALSE`, returns the unadjusted
  skewness (*g*₁) or kurtosis (*g*₂). (Note that `TRUE` corresponds to
  behavior of software such as SPSS, SAS, and Excel.)

- na.rm:

  Should missing values be removed? (Note that
  [`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md)
  automatically sets na.rm to `TRUE` for all functions).

- excess:

  If `TRUE`, returns excess kurtosis by (total kurtosis - 3).

- ...:

  A numeric vector or vectors.
