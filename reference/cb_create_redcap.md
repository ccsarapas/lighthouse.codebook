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

## Usage

``` r
cb_create_redcap(
  data,
  metadata,
  ...,
  .name = field_name,
  .var_label = field_label,
  .val_labels = select_choices_or_calculations,
  .form = form_name,
  .user_missing = NULL,
  .split_var_labels = NULL,
  .include_types = !.include_r_classes,
  .include_r_classes = FALSE,
  .val_labs_sep1 = ", ",
  .val_labs_sep2 = "\\|",
  .rmv_html = TRUE,
  .rmv_line_breaks = TRUE,
  .coerce_integers = TRUE,
  .checkbox_resp_values = FALSE,
  .propagate_checkbox_missings = TRUE,
  .user_missing_col = c("if_any", "yes", "no"),
  .user_missing_conflict = c("metadata", "missing_label"),
  .user_missing_incompatible = c("ignore", "warn", "error")
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

- .name, .var_label, .val_labels:

  Columns in `metadata` containing variable name, variable label, and
  value labels, respectively.

- .form:

  Column in `metadata` containing form names. (Set to `NULL` to omit.)

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

- .val_labs_sep1, .val_labs_sep2:

  Regex patterns separating value labels in `metadata`. `.val_labs_sep1`
  separates values from labels, and `.val_labs_sep2` separates
  value/label pairs. e.g., if value labels are in format
  `"1, First label|2, Second label"`, set `.val_labs_sep1` to `","` and
  `.val_labs_sep2` to `"\\|"`.

- .rmv_html:

  Should HTML tags be removed from metadata (e.g., from variable and
  value lables)?

- .rmv_line_breaks:

  Should line breaks be removed from metadata (e.g., from variable and
  value lables)? If `TRUE`, line breaks will be replaced with `" / "`.

- .coerce_integers:

  Should variables listed as "integer" in
  `metedata$text_validation_type_or_show_slider_number` be coerced to
  integer?

- .checkbox_resp_values:

  Should checkbox values use labels in `metadata` (`TRUE`) or "Yes" /
  "No" (`FALSE`)? See "Checkbox data handling" below.

- .propagate_checkbox_missings:

  Should user missing values in a checkbox group be propagated across
  all variables in the group? See "Checkbox data handling" below.

- .user_missing_col:

  Include value labels for user missing values in a separate column? The
  default, `"if_any"`, adds the column only if user missings are
  specified for at least one variable.

- .user_missing_conflict:

  If different labels for a value are provided in metadata and user
  missings, which should be used?

- .user_missing_incompatible:

  How to handle variables specified in `.user_missing` that aren't
  compatible with user missing values (e.g., logical, Date, or POSIXt)?

## Value

An `"li_codebook"` object, consisting of (1) a tibble summarizing the
passed dataset and (2) attributes containing the passed dataset (in
several formats) and additional metadata. Specifically:

- A tibble with columns:

  - `name`: variable name

  - `form`: form name

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

  - additional columns if specified in `...`

- Attributes:

  - Transformed versions of the passed dataset. See
    [`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md).

  - Lookup tables and other metadata used internally.

## Checkbox data handling

### Value labels

Data from REDCap checkboxes yields one variable in the dataset for each
response option. These will be labelled generically with `"Yes"` or
`"No"`, unless `.checkbox_resp_values` is `TRUE`, in which case
response-specific labels from `metadata` will be used. For example, if a
checkbox group has options "In the past year," "More than a year ago,"
and "Never," corresponding to variables `chk_var1___0`, `chk_var1___1`,
and `chk_var1___2`: if `.checkbox_resp_values` is `FALSE`, all of these
will have values:

- `chk_var1___0`, `chk_var1___1`, `chk_var1___2`: 0 = "No"; 1 = "Yes".

If `.checkbox_resp_values` is `TRUE`, each variable will have unique
labels:

- `chk_var1___0`: 0 = "Not selected," 1 = "In the past year"

- `chk_var1___1`: 0 = "Not selected," 1 = "More than a year ago"

- `chk_var1___2`: 0 = "Not selected," 0 = "Never"

### Missing value propagation

If `.propagate_checkbox_missings` is `TRUE`, missing values in a
checkbox group variable will be propagated to all variables in the
group. For example, given a checkbox group with options "Pregnant," "Not
pregnant," and "Not applicable," corresponding to variables
`chk_preg_0___0`, `chk_preg_0___1`, and `chk_preg_0____9`, and assuming
that `-9` is specified as a user missing value. If
`.propagate_checkbox_missings` is `TRUE`, `chk_preg_0___0` and
`chk_preg_0___1` will be set to `-9` if `chk_preg_0____9` is `1`.
Otherwise, these columns will remain as `0` where `chk_preg_0____9` is
`1`.
