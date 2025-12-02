# Extract data from a codebook object

Codebook objects created by
[`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
and friends contain several transformed versions of the originally
passed dataset. These can be extracted using `cb_get_data()`.

## Usage

``` r
cb_get_data(cb, format = c("factors", "haven", "values"))
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- format:

  Format of the returned data; see below for details.

## Value

A tibble with variables formatted based on the `format` argument.

- For `format = "values"`, all variables retain the same values as the
  original dataset, including values for user missings. The data may
  reflect transformations made by variants of
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  – e.g., for
  [`cb_create_redcap()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_redcap.md),
  integer coercion and propagation of user missings across checkbox
  variables.

- For `"haven"`, value labels and user missings are encoded using class
  [`"haven_labelled"`](https://haven.tidyverse.org/reference/labelled.html)\`

- For `"factors"`, all variables with value labels are converted to
  factors, and all user missings are converted to `NA`.
