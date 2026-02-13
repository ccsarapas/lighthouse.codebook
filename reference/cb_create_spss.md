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
  .options = cb_create_options()
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

- .options:

  Additional options to use for codebook creation. Must be the result
  from a call to
  [`cb_create_options()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md).
  See that function's help page for available options.

## Value

An `"li_codebook"` object, consisting of a tibble summarizing the passed
dataset and attributes containing additional metadata. The tibble
includes columns:

- `name`: variable name

- `type`: column containing simplified variable type

- `class`: optional column containing class(es) of each variable

- `label_stem`: optional column containing variable label stems, if any
  variables are specified in `.split_var_labels`

- `label`: variable label

- `values`: values, with labels if applicable

- `user_missing`: optional column showing user missing values, with
  labels if applicable. By default, this column is included only if user
  missings are specified for at least one variable. This behavior can be
  changed using the `user_missing_col` argument to
  [`cb_create_options()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md).

- `missing`: proportion missing
