# Generate a codebook object from REDCap data

`cb_create_redcap()` builds an object of class `"li_codebook"` from a
dataset and corresponding codebook exported from REDCap. The resulting
object can be used to write an Excel workbook with variable and data
summaries (using
[`cb_write()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_write.md)),
extract processed data
([`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)),
or generate dataset summaries
([`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md),
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md),
[`cb_summarize_text()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_text.md)).

This variant of
[`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
includes functionality specific to REDCap data and metadata, including:

- Defaults for typical REDCap metadata column names

- Includes form field by default

- Unpacking, labelling, and optional missing propagation for checkbox
  data

- Optional coercion for character variables marked as "integer" in
  `metedata$text_validation_type_or_show_slider_number`

All of these behaviors can be controlled using the `.options` argument.

## Usage

``` r
cb_create_redcap(
  data,
  metadata,
  ...,
  .user_missing = NULL,
  .split_var_labels = NULL,
  .options = cb_create_redcap_options()
)
```

## Arguments

- data:

  A data frame exported or retrieved from REDCap.

- metadata:

  A data frame containing the REDCap codebook associated with `data`.

- ...:

  Additional columns from `metadata` to preserve in the final codebook.
  New names can be assigned by passing named arguments. Columns for
  variable name, form, variable label, and value labels are included by
  default.

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
  [`cb_create_redcap_options()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md)
  or
  [`cb_create_options()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md).
  See
  [`?cb_create_redcap_options`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md)
  for available options.

## Value

An `"li_codebook"` object, consisting of a tibble summarizing the passed
dataset and attributes containing additional metadata. The tibble
includes columns:

- `name`: variable name

- `form`: form name

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

- additional columns if specified in `...`

## Checkbox data handling

### Value labels

Data from REDCap checkboxes yields one variable in the dataset for each
response option. By default, these will be labelled generically with
`"Yes"` or `"No"`. For example, consider a checkbox group with options
"In the past year," "More than a year ago," and "Never," corresponding
to variables `chk_var1___0`, `chk_var1___1`, and `chk_var1___2`. By
default, all of these will be given the same value labels:

- `chk_var1___0`, `chk_var1___1`, `chk_var1___2`: 0 = "No"; 1 = "Yes".
  This behavior can be changed by setting `checkbox_resp_values = TRUE`
  in
  [`cb_create_options()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md).
  In this case, response-specific labels from `metadata` will be used,
  so that each variable will have unique labels:

- `chk_var1___0`: 0 = "Not selected," 1 = "In the past year"

- `chk_var1___1`: 0 = "Not selected," 1 = "More than a year ago"

- `chk_var1___2`: 0 = "Not selected," 0 = "Never"

### Missing value propagation

By default, missing values in a checkbox group will be propagated to all
variables in the group. For example, consider a checkbox group with
options "Pregnant," "Not pregnant," and "Not applicable," corresponding
to variables `chk_preg_0___0`, `chk_preg_0___1`, and `chk_preg_0____9`,
and assuming that `-9` is specified as a user missing value. By default,
`chk_preg_0___0` and `chk_preg_0___1` will be set to `-9` if
`chk_preg_0____9` is `1`. This behavior can be overridden by setting
`propagate_checkbox_missings = FALSE` in
[`cb_create_options()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_options.md),
in which case no values will be changed.
