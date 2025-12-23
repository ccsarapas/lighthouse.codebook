# Summarize numeric variables from a codebook object

`cb_summarize_numeric()` generates a summary table for all numeric
variables from a codebook object, optionally by group. Future releases
will include options to specify the summary statistics used. Currently,
summary statistics are valid n and %; mean and SD; median, MAD, min,
max, and range; skewness, and kurtosis.

## Usage

``` r
cb_summarize_numeric(cb, group_by = NULL, warn_if_none = TRUE)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by.

- warn_if_none:

  Should a warning be issued if there are no numeric variables in `cb`?

## Value

If there no numeric variables in `cb`, `NULL`. Otherwise, a tibble with
columns:

- optional grouping column(s) if specified in `group_by`

- `name`: variable name

- `label_stem`: optional column containing variable label stems;
  included if `cb` includes a `label_stem` column and at least one
  numeric variable has a non-missing label stem.

- `label`: variable label

- `valid_n`, `valid_pct`: number and proportion of non-missing values

- summary statistic columns: by default, these include `mean` and
  standard deviation (`SD`); `median`, median absolute deviation
  (`MAD`), `min`, `max`, and `range`; skewness (`skew`), and kurtosis
  (`kurt`).
