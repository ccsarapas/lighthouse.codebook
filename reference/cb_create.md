# Generate a codebook object

`cb_create()` builds an object of class `"li_codebook"` from a dataset
and optional metadata. The resulting object can be used to write an
Excel workbook with variable and data summaries (using
[`cb_write()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_write.md)),
extract processed data
([`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)),
or generate dataset summaries
([`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md)
and
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md)).

## Usage

``` r
cb_create(
  data,
  metadata = NULL,
  ...,
  .name = name,
  .var_label = label,
  .val_labels = val_labels,
  .user_missing = NULL,
  .val_labs_sep1 = NULL,
  .val_labs_sep2 = NULL,
  .rmv_html = !name,
  .rmv_line_breaks = !name,
  .separate_missings = c("if_any", "yes", "no"),
  .user_missing_conflict = c("metadata", "missing_label")
)
```

## Arguments

- data:

  A data frame exported or retrieved from REDCap.

- metadata:

  A data frame containing metadata, such as variable labels and value
  labels.

- ...:

  Additional columns from `metadata` to preserve in the final codebook.
  New names can be assigned by passing named arguments. Columns for
  variable name, form, variable label, and value labels are included by
  default.

- .name, .var_label, .val_labels:

  Columns in `metadata` containing variable name, variable label, and
  value labels, respectively. If `metadata` is provided, `.name` must be
  specified. `.var_label` and `.val_labels` may be set to `NULL` to
  omit.

- .user_missing:

  A formula or list of formulas specifying user missing values. Formulas
  should specify variables on the left-hand side (as variable names or
  [tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  expressions), and missing values on the right-hand side. See
  "Specifying user missing values" below for examples.

- .val_labs_sep1, .val_labs_sep2:

  Regex patterns separating value labels in `metadata`. `.val_labs_sep1`
  separates values from labels, and `.val_labs_sep2` separates
  value/label pairs. e.g., if value labels are in format
  `"1, First label|2, Second label"`, set `.val_labs_sep1` to `","` and
  `.val_labs_sep2` to `"\\|"`.

- .rmv_html:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Codebook columns from which HTML tags should be removed.

- .rmv_line_breaks:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Codebook columns from which line breaks should be removed.

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

    cb <- cb_create_redcap(data, metadata, .user_missing = var1 ~ 99)

The same user missings can be applied to multiple variables using
[tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
expressions:

    # for variables `var1` through `var5`
    .user_missing = var1:var5 ~ 99

    # for all numeric variables, plus `var6` and `var7`
    .user_missing = c(where(is.numeric), var6, var7) ~ c(-9, -8, -7)

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
