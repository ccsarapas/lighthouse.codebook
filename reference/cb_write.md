# Write codebook and data summaries to an Excel workbook

`cb_write()` writes an Excel workbook to disk with tabs including a
codebook; summary statistics for numeric variables; frequencies for
categorical variables; and optional grouped data summaries. For data
summaires, variables with value labels, factors (including ordered
factors), and logical variables are treated as categorical, while
numeric and integer variables are treated as numeric.

## Usage

``` r
cb_write(
  cb,
  file,
  dataset_name = NULL,
  incl_date = TRUE,
  incl_dims = TRUE,
  group_by = NULL,
  detail_missing = TRUE,
  overwrite = TRUE
)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- file:

  Path to write to.

- dataset_name:

  Name of the dataset to display in workbook headers.

- incl_date, incl_dims:

  Should the date and/or dataset dimensions be included in the Overview
  tab header?

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by. If specified, additional numeric and
  categorical summary tabs will be included with decked heads for
  specified groups.

- detail_missing:

  Include detailed missing value information on categorical summary tab?

- overwrite:

  Overwrite existing file?

## Value

Invisibly returns the path to the written Excel file.
