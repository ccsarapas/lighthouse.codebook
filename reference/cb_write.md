# Write codebook and data summaries to an Excel workbook

`cb_write()` writes an Excel workbook to disk with tabs including a
codebook; summary statistics for numeric variables; frequencies for
categorical variables; truncated frequencies for text variables; and
optional grouped summaries for numeric and categorical variables.

For data summaries, variables with value labels, factors, and logical
variables are treated as categorical; numeric and integer variables are
treated as numeric; and (unlabeled) character variables are treated as
text. Summary tabs will be omitted if there are no variables of the
relevant type.

## Usage

``` r
cb_write(
  cb,
  file,
  dataset_name = NULL,
  incl_date = TRUE,
  incl_dims = TRUE,
  hyperlinks = TRUE,
  group_by = NULL,
  group_rows_numeric = NULL,
  detail_missing = c("if_any_user_missing", "yes", "no"),
  n_text_vals = 5,
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

- hyperlinks:

  If `TRUE`, variable names on the Overview sheet will link to
  corresponding rows on summary tabs and vice versa.

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by. If specified, additional numeric and
  categorical summary tabs will be included with grouped summaries.
  Subgroups are shown in columns by default. For the numeric summary
  tab, subgroups for some or all grouping variables can instead be shown
  in rows if specified in `group_rows_numeric`.

- group_rows_numeric:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by in rows on the grouped numeric summary
  tab. All columns must also be specified in `group_by`.

- detail_missing:

  Include detailed missing value information on ungrouped categorical
  and text summary tabs? (Detailed missing information for grouped
  summary tabs is not currently supported.)

- n_text_vals:

  On the text summary tab, how many unique non-missing values should be
  included for each variable? If there are more than `n_text_vals` + 1
  unique values, the `n_text_vals` most common non-missing values will
  be included.

- overwrite:

  Overwrite existing file?

## Value

Invisibly returns the path to the written Excel file. The Excel workbook
itself will contain an "Overview" tab (see
[`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
and variants for details); "Summary - Numeric", "Summary - Categorical",
and "Summary - Text" tabs if the dataset includes variables of the
relevant type (see
[`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md),
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md),
and
[`cb_summarize_text()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_text.md));
and "Grouped Summary - Numeric" and "Grouped Summary - Categorical" tabs
if `group_by` is specified.

## Alternative labelling for user missing values

Different terminology for user missing values can be used by setting
`options(lighthouse.codebook.nonresponse = TRUE)`. If set, the "User
Missings" column on the Overview sheet is instead called "Nonresponse
Codes," and "Missing" values are instead referred to as "Excluded"
values on overview and summary tabs.
