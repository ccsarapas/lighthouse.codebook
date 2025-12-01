# Generate a codebook object from REDCap data

`cb_create_rc()` builds an object of class `"li_codebook"` from a
dataset and corresponding codebook exported from REDCap. The resulting
object can be used to write an Excel workbook with variable and data
summaries (using
[`cb_write()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_write.md)),
extract processed data
([`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)),
or generate dataset summaries
([`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md)
and
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md)).

This variant of `cb_create()` includes functionality specific to REDCap
data and metadata, including:

- Defaults for typical REDCap metadata column names

- Includes form field by default

- Unpacking, labelling, and optional missing propagation for checkbox
  data

- Optional coercion for character variables marked as "integer" in
  `metedata$text_validation_type_or_show_slider_number`

## Usage

``` r
cb_create_rc(
  data,
  metadata,
  ...,
  .name = field_name,
  .var_label = field_label,
  .val_labels = select_choices_or_calculations,
  .form = form_name,
  .user_missing = NULL,
  .val_labs_sep1 = ", ",
  .val_labs_sep2 = "\\|",
  .rmv_html = !name,
  .rmv_line_breaks = !name,
  .coerce_integers = TRUE,
  .checkbox_resp_values = FALSE,
  .propagate_checkbox_missings = TRUE,
  .separate_missings = c("if_any", "yes", "no"),
  .user_missing_conflict = c("metadata", "missing_label")
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
  expressions), and missing values on the right-hand side. See
  "Specifying user missing values" below for examples.

- .val_labs_sep1, .val_labs_sep2:

  Regex patterns separating value labels in `metadata`. e.g., if value
  labels are in format `"1, First label|2, Second label"`, set
  `.val_labs_sep1` to `","` and `.val_labs_sep2` to `"\\|"`.

- .rmv_html:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Codebook columns from which HTML tags should be removed.

- .rmv_line_breaks:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Codebook columns from which line breaks should be removed.

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

- .separate_missings:

  Include value labels for user missing values in a separate column? The
  default, `"if_any"`, adds the column only if user missings are
  specified for at least one variable.

- .user_missing_conflict:

  If different labels for a value are provided in metadata and user
  missings, which should be used?

## Value

An `"li_codebook"` object, consisting of (1) a tibble summarizing the
passed dataset and (2) attributes containing the passed dataset (in
several formats) and additional metadata. Specifically:

- A tibble with columns:

  - `name`: variable name

  - `form`: form name

  - `type`: variable type

  - `label`: variable label

  - `value_labels`: value labels

  - `user_missing`: optional column, depending on value of
    `.separate_missings`, with value labels for user missing values

  - `missing`: proportion missing

  - additional columns if specified in `...`

- Attributes:

  - Transformed versions of the passed dataset. See
    [`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md).

  - Lookup tables and other metadata used internally: `"user_missing"`,
    `"vals_by_label"`, `"labs_by_value"`, `"miss_propagate"`,
    `"factors"`, `"n_obs"`, `"n_vars"`

## Specifying user missing values

User missing values are defined by passing a formula or list of formulas
to the `.user_missing` argument. Formulas should specify variables on
the left-hand side and user missing values for those variables on the
right-hand side:

    cb <- cb_create_rc(data, metadata, .user_missing = var1 ~ 99)

The same user missings can be applied to multiple variables using
[tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
expressions:

    # for variables `var1` through `var5`
    .user_missing = var1:var5 ~ 99

    # for all numeric variables, plus `var6` and `var7`
    .user_missing = c(where(is.numeric), var6, var7) ~ c(-9, -8, -7)

    # for all variables
    .user_missing = everything() ~ c(-9, -8)

Different user missings can be applied to different variables using a
list of formulas:

    .user_missing = list(
      starts_with("status") ~ c(98, 99),
      var7:var10 ~ 97
    )

User missing values may optionally be named to set value labels:

    .user_missing = where(is.numeric) ~
      c(Declined = -97, "Don't know" = -98, "Not applicable" = -99)

If labels set in `.user_missing` conflict with those in `metadata`,
`.user_missing_conflict` controls which labels are used.

## Checkbox data handling

### Value labels

Data from REDCap checkboxes yields one variable in the dataset for each
response option. These will be labelled generically with `"Yes"` or
`"No"`, unless `.checkbox_resp_values` is `TRUE`, in which case
response-specific labels from `metadata` will be used. For example, if a
checkbox group has options "In the past year," "More than a year ago,"
and "Never," corresponding to variables `chk_var1___0`, `chk_var1___1`,
and `chk_var1___2`: if `.checkbox_resp_values` is `FALSE`, all of these
will have values "\[0\] No; \[1\] Yes." If `.checkbox_resp_values` is
`TRUE`, each variable: will have unique labels:

- `chk_var1___0`: "\[0\] Not selected \[1\] In the past year"

- `chk_var1___1`: "\[0\] Not selected \[1\] More than a year ago"

- `chk_var1___2`: "\[0\] Not selected \[1\] Never"

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

\[0\]: R:0%5C \[1\]: R:1%5C \[0\]: R:0 \[1\]: R:1 \[0\]: R:0 \[1\]: R:1
\[0\]: R:0 \[1\]: R:1
