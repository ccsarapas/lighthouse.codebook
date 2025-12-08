# Generate a codebook object from an SPSS dataset

`cb_create_spss()` builds an object of class `"li_codebook"` from an
SPSS dataset (imported using
[`haven::read_spss()`](https://haven.tidyverse.org/reference/read_spss.html),
`read_sav()`, or `read_por()`). Metadata including variable labels,
value labels, and user missing values are extracted from the imported
dataset. (User missing values can also be set using the `.user_missing`
argument.)\`

The resulting object can be used to write an Excel workbook with
variable and data summaries (using
[`cb_write()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_write.md)),
extract processed data
([`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)),
or generate dataset summaries
([`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md)
and
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md)).

## Usage

``` r
cb_create_spss(
  data,
  .user_missing = NULL,
  .separate_missings = c("if_any", "yes", "no"),
  .user_missing_conflict = c("val_label", "missing_label")
)
```

## Arguments

- data:

  A data frame exported or retrieved from REDCap.

- .user_missing:

  A formula or list of formulas specifying user missing values. Formulas
  should specify variables on the left-hand side (as variable names or
  [tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expressions), and missing values on the right-hand side. See
  "Specifying user missing values" in
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  documentation for examples.

- .separate_missings:

  Include value labels for user missing values in a separate column? The
  default, `"if_any"`, adds the column only if user missings are
  specified for at least one variable.

- .user_missing_conflict:

  If labels passed to `.user_missing` conflicts with a value label in
  the `data`, which should be used?

## Value

An `"li_codebook"` object, consisting of (1) a tibble summarizing the
passed dataset and (2) attributes containing the passed dataset (in
several formats) and additional metadata. Specifically:

- A tibble with columns:

  - `name`: variable name

  - `type`: variable type

  - `label`: variable label

  - `value_labels`: value labels

  - `user_missing`: optional column, depending on value of
    `.separate_missings`, with value labels for user missing values

  - `missing`: proportion missing

- Attributes:

  - Transformed versions of the passed dataset. See
    [`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md).

  - Lookup tables and other metadata used internally: `"user_missing"`,
    `"vals_by_label"`, `"labs_by_value"`, `"miss_propagate"`,
    `"factors"`, `"n_obs"`, `"n_vars"`\#'
