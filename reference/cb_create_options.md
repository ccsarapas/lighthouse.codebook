# Additional options for codebook creation

Additional options for use by
[`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md).

## Usage

``` r
cb_create_options(
  ...,
  include_types = TRUE,
  include_r_classes = FALSE,
  rmv_html = TRUE,
  rmv_line_breaks = TRUE,
  user_missing_col = c("if_any", "yes", "no"),
  user_missing_conflict = c("val_label", "missing_label"),
  user_missing_incompatible = c("ignore", "warn", "error")
)

cb_create_redcap_options(
  ...,
  include_types = TRUE,
  include_r_classes = FALSE,
  rmv_html = TRUE,
  rmv_line_breaks = TRUE,
  user_missing_col = c("if_any", "yes", "no"),
  user_missing_conflict = c("val_label", "missing_label"),
  user_missing_incompatible = c("ignore", "warn", "error"),
  name = field_name,
  var_label = field_label,
  val_labels = select_choices_or_calculations,
  type = field_type,
  form = form_name,
  val_labs_sep1 = ", ",
  val_labs_sep2 = "\\|",
  coerce_integers = TRUE,
  checkbox_resp_values = FALSE,
  propagate_checkbox_missings = TRUE
)
```

## Arguments

- ...:

  These dots are for future extensions and must be empty.

- include_types:

  Include a column listing simplified type for each variable? (e.g,.
  `"categorical"`, `"date-time"`.)

- include_r_classes:

  Include a column listing class(es) of each variable? (e.g.,
  `"factor"`, `"POSIXct, POSIXt"`.)

- rmv_html:

  Should HTML tags be removed from metadata (e.g., from variable and
  value labels)?

- rmv_line_breaks:

  Should line breaks be removed from metadata (e.g., from variable and
  value labels)? If `TRUE`, line breaks will be replaced with `" / "`.

- user_missing_col:

  Include value labels for user missing values in a separate column? The
  default, `"if_any"`, adds the column only if user missings are
  specified for at least one variable.

- user_missing_conflict:

  If labels passed to `.user_missing` conflict with value labels in
  metadata, which should be used?

- user_missing_incompatible:

  How to handle variables specified in `.user_missing` that aren't
  compatible with user missing values (e.g., logical, Date, or POSIXt)?

- name, var_label, val_labels, type:

  For REDCap data, columns in `metadata` containing variable name,
  variable label, value labels, and variable type, respectively.

- form:

  For REDCap data, column in `metadata` containing form names. (Set to
  `NULL` to omit.)

- val_labs_sep1, val_labs_sep2:

  For REDCap data, regex patterns separating value labels in `metadata`.
  `val_labs_sep1` separates values from labels, and `val_labs_sep2`
  separates value/label pairs from one another. e.g., if value labels
  are in the format `"1, First label|2, Second label"`, set
  `val_labs_sep1` to `","` and `val_labs_sep2` to `"\\|"`.

- coerce_integers:

  For REDCap data, should variables listed as "integer" in
  `metedata$text_validation_type_or_show_slider_number` be coerced to
  integer?

- checkbox_resp_values:

  For REDCap data, should checkbox values use labels in `metadata`
  (`TRUE`) or "Yes" / "No" (`FALSE`)? See "Checkbox data handling" on
  the
  [`cb_create_redcap()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_redcap.md)
  help page.

- propagate_checkbox_missings:

  For REDCap data, should user missing values in a checkbox group be
  propagated across all variables in the group? See "Checkbox data
  handling" on the
  [`cb_create_redcap()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_redcap.md)
  help page.
