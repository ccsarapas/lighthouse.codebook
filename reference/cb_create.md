# Generate a codebook object

`cb_create()` builds an object of class `"li_codebook"` from a dataset
and optional metadata. The resulting object can be used to write an
Excel workbook with variable and data summaries (using
[`cb_write()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_write.md)),
extract processed data
([`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)),
or generate dataset summaries
([`cb_summarize_numeric()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_numeric.md),
[`cb_summarize_categorical()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_categorical.md),
[`cb_summarize_text()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_summarize_text.md)).

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
  .split_var_labels = NULL,
  .include_types = !.include_r_classes,
  .include_r_classes = FALSE,
  .val_labs_sep1 = NULL,
  .val_labs_sep2 = NULL,
  .rmv_html = TRUE,
  .rmv_line_breaks = TRUE,
  .user_missing_col = c("if_any", "yes", "no"),
  .user_missing_conflict = c("metadata", "missing_label"),
  .user_missing_incompatible = c("ignore", "warn", "error")
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
  expressions), and missing values on the right-hand side. If left-hand
  side is omitted, defaults to
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  See "Specifying user missing values" below for examples.

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

  - `type`: optional column containing simplified variable type

  - `class`: optional column containing class(es) of each variable

  - `label_stem`: optional column containing variable label stems, if
    any variables are specified in `.split_var_labels`

  - `label`: variable label

  - `value_labels`: value labels

  - `user_missing`: optional column, depending on value of
    `.user_missing_col`, with value labels for user missing values

  - `missing`: proportion missing

  - additional columns if specified in `...`

- Attributes:

  - Transformed versions of the passed dataset. See
    [`cb_get_data()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_get_data.md)

  - Lookup tables and other metadata used internally.

## Specifying user missing values

User missing values are defined by passing a formula or list of formulas
to the `.user_missing` argument. Formulas should specify variables on
the left-hand side and user missing values for those variables on the
right-hand side:

    cb <- cb_create(data, metadata, .user_missing = var1 ~ 99)

The same user missings can be applied to multiple variables using
[tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
expressions.

    # for variables `var1` through `var5`
    .user_missing = var1:var5 ~ 99

    # for all numeric variables, plus `var6` and `var7`
    .user_missing = c(where(is.numeric), var6, var7) ~ c(-9, -8, -7)

    # omitted left-hand side defaults to `tidyselect::everything()`
    .user_missing = ~ -99

Different user missings can be applied to different variables using a
list of formulas:

    .user_missing = list(
      starts_with("status") ~ c(98, 99),
      var7:var10 ~ 97
    )

User missing values may optionally be named to set value labels:

    .user_missing = ~ c(Declined = -98, "Not applicable" = -99)

If labels set in `.user_missing` conflict with those in `metadata`,
`.user_missing_conflict` controls which labels are used.

User missing values are not compatible with logical, date, or datetime
(POSIXt) variables. By default, these variables will be ignored if
specified in `.user_missing`. (i.e., user missing values will be applied
only to compatible variables.) This behavior can be changed using the
`.user_missing_incompatible` argument.

## Examples

``` r
diamonds2 <- ggplot2::diamonds |>
  transform(
    carat_group = as.integer(cut(carat, breaks = 3, labels = 1:3)),
    price_group = as.integer(cut(
      price,
      breaks = c(0, 500, 1000, 2000, 5000, 10000, Inf),
      labels = 1:6,
      right = FALSE
    ))
  )

# basic codebook
cb_create(diamonds2)
#> # A tibble: 12 × 4
#>    name        type    value_labels                           missing
#>    <chr>       <chr>   <chr>                                    <dbl>
#>  1 carat       numeric NA                                           0
#>  2 cut         ordinal Fair; Good; Very Good; Premium; Ideal        0
#>  3 color       ordinal D; E; F; G; H; I; J                          0
#>  4 clarity     ordinal I1; SI2; SI1; VS2; VS1; VVS2; VVS1; IF       0
#>  5 depth       numeric NA                                           0
#>  6 table       numeric NA                                           0
#>  7 price       integer NA                                           0
#>  8 x           numeric NA                                           0
#>  9 y           numeric NA                                           0
#> 10 z           numeric NA                                           0
#> 11 carat_group integer NA                                           0
#> 12 price_group integer NA                                           0

# convert variables to factor to treat as categorical
diamonds2 |>
  transform(
    carat_group = factor(carat_group),
    price_group = factor(price_group)
  ) |> 
  cb_create()
#> # A tibble: 12 × 4
#>    name        type        value_labels                           missing
#>    <chr>       <chr>       <chr>                                    <dbl>
#>  1 carat       numeric     NA                                           0
#>  2 cut         ordinal     Fair; Good; Very Good; Premium; Ideal        0
#>  3 color       ordinal     D; E; F; G; H; I; J                          0
#>  4 clarity     ordinal     I1; SI2; SI1; VS2; VS1; VVS2; VVS1; IF       0
#>  5 depth       numeric     NA                                           0
#>  6 table       numeric     NA                                           0
#>  7 price       integer     NA                                           0
#>  8 x           numeric     NA                                           0
#>  9 y           numeric     NA                                           0
#> 10 z           numeric     NA                                           0
#> 11 carat_group categorical 1; 2; 3                                      0
#> 12 price_group categorical 1; 2; 3; 4; 5; 6                             0

# provide metadata for variable and value labels
diamonds_meta <- data.frame(
  name = names(diamonds2),
  label = c(
    # from ?ggplot2::diamonds
    "price in US dollars ($326–$18,823)",
    "weight of the diamond (0.2–5.01)",
    "quality of the cut (Fair, Good, Very Good, Premium, Ideal)",
    "diamond colour, from D (best) to J (worst)",
    "a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))",
    "length in mm (0–10.74)",
    "width in mm (0–58.9)",
    "depth in mm (0–31.8)",
    "total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)",
    "width of top of diamond relative to widest point (43–95)",
    "diamond carat (3 groups)",
    "diamond price (6 groups)"
  ),
  val_labels = c(
    rep(NA, 10),
    "1 = small; 2 = medium; 3 = large",
    "1 = <$500; 2 = $500-$999; 3 = $1,000-$1,999; 4 = $2,000-$4,999; 5 = $5,000-$9,999; 6 = $10,000+"
  )
)

cb_create(
  diamonds2, diamonds_meta,
  .val_labs_sep1 = " = ", .val_labs_sep2 = "; "
)
#> # A tibble: 12 × 5
#>    name        type        label                            value_labels missing
#>    <chr>       <chr>       <chr>                            <chr>          <dbl>
#>  1 carat       numeric     price in US dollars ($326–$18,8… NA                 0
#>  2 cut         ordinal     weight of the diamond (0.2–5.01) Fair; Good;…       0
#>  3 color       ordinal     quality of the cut (Fair, Good,… D; E; F; G;…       0
#>  4 clarity     ordinal     diamond colour, from D (best) t… I1; SI2; SI…       0
#>  5 depth       numeric     a measurement of how clear the … NA                 0
#>  6 table       numeric     length in mm (0–10.74)           NA                 0
#>  7 price       integer     width in mm (0–58.9)             NA                 0
#>  8 x           numeric     depth in mm (0–31.8)             NA                 0
#>  9 y           numeric     total depth percentage = z / me… NA                 0
#> 10 z           numeric     width of top of diamond relativ… NA                 0
#> 11 carat_group categorical diamond carat (3 groups)         [1] small; …       0
#> 12 price_group categorical diamond price (6 groups)         [1] <$500; …       0
```
