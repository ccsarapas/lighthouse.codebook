# Generate a codebook object from an SPSS dataset

`cb_create_spss()` builds an object of class `"li_codebook"` from an
imported SPSS dataset. Metadata including variable labels, value labels,
and user missing values are extracted from the imported dataset. (User
missing values can also be set using the `.user_missing` argument.)\`

The resulting object can be used to write an Excel workbook with
variable and data summaries (using
[`cb_write()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_write.md)),
extract processed data
([`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)),
or generate dataset summaries
([`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md),
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md),
[`cb_summarize_text()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_text.md)).

## Usage

``` r
cb_create_spss(
  data,
  .user_missing = NULL,
  .split_var_labels = NULL,
  .include_types = !.include_r_classes,
  .include_r_classes = FALSE,
  .rmv_html = TRUE,
  .rmv_line_breaks = TRUE,
  .user_missing_col = c("if_any", "yes", "no"),
  .user_missing_conflict = c("val_label", "missing_label"),
  .user_missing_incompatible = c("ignore", "warn", "error")
)
```

## Arguments

- data:

  A data frame imported from SPSS using (imported using
  [`haven::read_spss()`](https://haven.tidyverse.org/reference/read_spss.html),
  `read_sav()`, or `read_por()`).

- .user_missing:

  A formula or list of formulas specifying user missing values. Formulas
  should specify variables on the left-hand side (as variable names or
  [tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expressions), and missing values on the right-hand side. If left-hand
  side is omitted, defaults to
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  See "Specifying user missing values" in
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  documentation for examples.

- .split_var_labels:

  A
  [`tidyselect`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expression or list of tidyselect expressions, indicating (sets of)
  variable labels with a common stem that should be extracted into a
  separate column.

- .include_types:

  Include a column listing simplified type for each variable? (e.g,.
  `"categorical"`, `"date-time"`.)

- .include_r_classes:

  Include a column listing class(es) of each variable? (e.g.,
  `"factor"`, `"POSIXct, POSIXt"`.)

- .rmv_html:

  Should HTML tags be removed from variable and value labels?

- .rmv_line_breaks:

  Should line breaks be removed from variable and value labels? If
  `TRUE`, line breaks will be replaced with `" / "`.

- .user_missing_col:

  Include value labels for user missing values in a separate column? The
  default, `"if_any"`, adds the column only if user missings are
  specified for at least one variable.

- .user_missing_conflict:

  If labels passed to `.user_missing` conflicts with a value label in
  `data`, which should be used?

- .user_missing_incompatible:

  How to handle variables specified in `.user_missing` that aren't
  compatible with user missing values (e.g., logical, Date, or POSIXt)?

## Value

An `"li_codebook"` object, consisting of (1) a tibble summarizing the
passed dataset and (2) attributes containing the passed dataset (in
several formats) and additional metadata. Specifically:

- A tibble with columns:

  - `name`: variable name

  - `type`: optional column containing simplified variable type

  - `class`: optional column containing class(es) of each variable

  - `label_stem`: optional column containing variable label stems, if
    any variables are specified in `.split_var_labels`

  - `label`: variable label

  - `values`: values, with labels if applicable

  - `user_missing`: optional column, depending on value of
    `.user_missing_col`, showing user missing values, with labels if
    applicable

  - `missing`: proportion missing

- Attributes:

  - Transformed versions of the passed dataset. See
    [`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md).

  - Lookup tables and other metadata used internally.
