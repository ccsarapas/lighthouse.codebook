# Summarize character variables from a codebook object

`cb_summarize_text()` generates a summary table for all character
variables from a codebook object, including number of unique values,
frequencies for the most common values, and missing value information.
Note that character variables of class `"haven_labelled"` are treated as
categorical; see
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md).

## Usage

``` r
cb_summarize_text(
  cb,
  n_text_vals = 5,
  detail_missing = TRUE,
  detail_na_label = "NA",
  warn_if_none = TRUE
)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- n_text_vals:

  How many unique non-missing values should be included for each
  variable?

- detail_missing:

  Include detailed missing value information?

- detail_na_label:

  Label used for `NA` values when `detail_missing` is `TRUE`.

- warn_if_none:

  Should a warning be issued if there are no text variables in `cb`?

## Value

If there no text variables in `cb`, `NULL`. Otherwise, a tibble with
columns:

- `name`: variable name

- `label_stem`: optional column containing variable label stems;
  included if `cb` includes a `label_stem` column and at least one
  character variable has a non-missing label stem.

- `label`: variable label

- `unique_n`: number of unique non-missing values

- `is_missing`: optional column indicating if `value` is a missing
  value. Included if `detail_missing` is `TRUE`.

- `value`: the most prevalent unique values for the variable. If there
  are more than `n_text_vals` + 1 unique values, the `n_text_vals` most
  common non-missing values will be included. (All missing values will
  always be included.)

- `n`: number of observations

- `pct_of_all`: proportion of all (non-missing and missing) observations

- `pct_of_valid`: for non-missing values, proportion of all non-missing
  observations

- `pct_of_missing`: optional column showing, for missing values,
  proportion of all missing observations. Included if `detail_missing`
  is `TRUE`.
