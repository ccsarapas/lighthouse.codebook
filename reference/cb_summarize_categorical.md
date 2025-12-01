# Summarize categorical variables from a codebook object

`cb_summarize_categorical()` generates a frequencies table for all
categorical variables from a codebook object, optionally by group.

## Usage

``` r
cb_summarize_categorical(
  cb,
  group_by = NULL,
  prefixed = TRUE,
  detail_missing = missing(group_by),
  detail_na_label = "NA"
)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by `cb_create()` or a
  variant.

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by.

- prefixed:

  Should value labels be prefixed with the corresponding value? e.g.,
  `TRUE` yields ``` "[1] Value One"``;  ```FALSE`yields`"Value One"\`.

- detail_missing:

  Include detailed missing value information? Currently supported only
  when no grouping variables are specified.

- detail_na_label:

  Label used for `NA` values when `detail_missing` is `TRUE`.

## Value

A tibble with frequency information for each categorical variable.
