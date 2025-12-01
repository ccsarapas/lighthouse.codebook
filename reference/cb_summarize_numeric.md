# Summarize numeric variables from a codebook object

`cb_summarize_numeric()` generates a summary table for all numeric
variables from a codebook object, optionally by group. Future releases
will include options to specify the summary statistics used. Currently,
summary statistics are valid n and %; mean and SD; median, MAD, min,
max, and range; skewness, and kurtosis.

## Usage

``` r
cb_summarize_numeric(cb, group_by = NULL)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by `cb_create()` or a
  variant.

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by.

## Value

A tibble with summary statistics for each numeric variable.
