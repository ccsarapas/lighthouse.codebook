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
#>  2 cut         ordered Fair; Good; Very Good; Premium; Ideal        0
#>  3 color       ordered D; E; F; G; H; I; J                          0
#>  4 clarity     ordered I1; SI2; SI1; VS2; VS1; VVS2; VVS1; IF       0
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
#>    name        type    value_labels                           missing
#>    <chr>       <chr>   <chr>                                    <dbl>
#>  1 carat       numeric NA                                           0
#>  2 cut         ordered Fair; Good; Very Good; Premium; Ideal        0
#>  3 color       ordered D; E; F; G; H; I; J                          0
#>  4 clarity     ordered I1; SI2; SI1; VS2; VS1; VVS2; VVS1; IF       0
#>  5 depth       numeric NA                                           0
#>  6 table       numeric NA                                           0
#>  7 price       integer NA                                           0
#>  8 x           numeric NA                                           0
#>  9 y           numeric NA                                           0
#> 10 z           numeric NA                                           0
#> 11 carat_group factor  1; 2; 3                                      0
#> 12 price_group factor  1; 2; 3; 4; 5; 6                             0

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
#>    name        type    label                                value_labels missing
#>    <chr>       <chr>   <chr>                                <chr>          <dbl>
#>  1 carat       numeric price in US dollars ($326–$18,823)   NA                 0
#>  2 cut         ordered weight of the diamond (0.2–5.01)     Fair; Good;…       0
#>  3 color       ordered quality of the cut (Fair, Good, Ver… D; E; F; G;…       0
#>  4 clarity     ordered diamond colour, from D (best) to J … I1; SI2; SI…       0
#>  5 depth       numeric a measurement of how clear the diam… NA                 0
#>  6 table       numeric length in mm (0–10.74)               NA                 0
#>  7 price       integer width in mm (0–58.9)                 NA                 0
#>  8 x           numeric depth in mm (0–31.8)                 NA                 0
#>  9 y           numeric total depth percentage = z / mean(x… NA                 0
#> 10 z           numeric width of top of diamond relative to… NA                 0
#> 11 carat_group factor  diamond carat (3 groups)             [1] small; …       0
#> 12 price_group factor  diamond price (6 groups)             [1] <$500; …       0
```
