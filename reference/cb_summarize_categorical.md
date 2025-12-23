# Summarize categorical variables from a codebook object

`cb_summarize_categorical()` generates a frequencies table for all
categorical variables from a codebook object, optionally by group.
Variables with value labels, factors (including ordered factors), and
logical variables are treated as categorical.

## Usage

``` r
cb_summarize_categorical(
  cb,
  group_by = NULL,
  prefixed = TRUE,
  detail_missing = missing(group_by),
  detail_na_label = "NA",
  warn_if_none = TRUE
)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by.

- prefixed:

  Should value labels be prefixed with the corresponding value? e.g.,
  `TRUE` yields `"[1] Value One"`; `FALSE` yields `"Value One"`.

- detail_missing:

  Include detailed missing value information? Currently supported only
  when no grouping variables are specified.

- detail_na_label:

  Label used for `NA` values when `detail_missing` is `TRUE`.

- warn_if_none:

  Should a warning be issued if there are no categorical variables in
  `cb`?

## Value

If there no categorical variables in `cb`, `NULL`. Otherwise, a tibble
with columns:

- optional grouping column(s) if specified in `group_by`

- `name`: variable name

- `label_stem`: optional column containing variable label stems;
  included if `cb` includes a `label_stem` column and at least one
  categorical variable has a non-missing label stem.

- `label`: variable label

- `is_missing`: optional column indicating if `value` is a missing
  value. Included if `detail_missing` is `TRUE`.

- `value`: variable value

- `n`: number of observations

- `pct_of_all`: proportion of all (non-missing and missing) observations

- `pct_of_valid`: for non-missing values, proportion of all non-missing
  observations

- `pct_of_missing`: optional column showing, for missing values,
  proportion of all missing observations. Included if `detail_missing`
  is `TRUE`.
