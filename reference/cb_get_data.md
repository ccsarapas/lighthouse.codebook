# Extract data from a codebook object

Codebook objects created by
[`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
and friends contain several transformed versions of the originally
passed dataset. These can be extracted using `cb_get_data()`.

## Usage

``` r
cb_get_data(cb, format = c("factors", "haven"))
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- format:

  Format of the returned data, either `"factors"` or `"haven"`; see
  below for details.

## Value

A tibble with variables formatted based on the `format` argument.

- For `"factors"`, all variables with value labels are converted to
  factors, and all user missings are converted to `NA`.

- For `"haven"`, variable labels, value labels, and user missings are
  encoded using class
  [`"haven_labelled_spss"`](https://haven.tidyverse.org/reference/labelled.html)\`.

Both formats may also reflect transformations made by variants of
[`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md).
In particular, for codebooks created using
[`cb_create_redcap()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create_redcap.md),
integer coercion and propagation of user missings across checkbox
variables.
