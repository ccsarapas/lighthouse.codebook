# Summarize numeric variables from a codebook object

`cb_summarize_numeric()` generates a summary table for all numeric
variables from a codebook object, optionally by group.

## Usage

``` r
cb_summarize_numeric(
  cb,
  group_by = NULL,
  stats = list(mean = mean, SD = sd, median = median, MAD = mad, min = min_if_any, max =
    max_if_any, range = spread, skew = skew, kurt = kurtosis),
  warn_if_none = TRUE
)
```

## Arguments

- cb:

  An object of class `"li_codebook"` as produced by
  [`cb_create()`](https://ccsarapas.github.io/lighthouse.codebook/reference/cb_create.md)
  or a variant.

- group_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column or columns to group by.

- stats:

  A named list of summary functions to include. The defaults include
  mean and standard deviation (SD); median and median absolute deviation
  (MAD); minimum, maximum, and range; and adjusted skewness and
  kurtosis. See details and examples.

- warn_if_none:

  Should a warning be issued if there are no numeric variables in `cb`?

## Value

If there no numeric variables in `cb`, `NULL`. Otherwise, a tibble with
columns:

- optional grouping column(s) if specified in `group_by`

- `name`: variable name

- `label_stem`: optional column containing variable label stems;
  included if `cb` includes a `label_stem` column and at least one
  numeric variable has a non-missing label stem.

- `label`: variable label

- `valid_n`, `valid_pct`: number and proportion of non-missing values

- Summary statistic columns as specified in `stats`

## Details

The `stats` argument controls which summary statistics will be computed.
It takes a named list of functions, where the names will be used as
column names.

`cb_summarize_numeric()` will set `na.rm` to `TRUE` for any function
that takes a `na.rm` argument.

You can include anonymous functions. If wrapping a function that takes a
`na.rm` argument, it is recommended you explicitly set `na.rm` to
`TRUE`. (e.g., to include the 25th quantile, use
`q25 = \(x) quantile(x, 0.25, na.rm = TRUE)`.

## Examples

``` r
cb_storms <- dplyr::storms |>
  dplyr::mutate(year = factor(year)) |>
  dplyr::filter(status %in% c("tropical storm", "hurricane")) |>
  cb_create()

# ungrouped summary with default stats
cb_summarize_numeric(cb_storms)
#> # A tibble: 10 × 12
#>    name        valid_n valid_pct   mean     SD median    MAD    min    max range
#>    <chr>         <int>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>
#>  1 month         12352     1       8.80   1.22      9   1.48    1     12    11  
#>  2 day           12352     1      15.7    8.98     16  11.9     1     31    30  
#>  3 hour          12352     1       9.14   6.72     12   8.90    0     23    23  
#>  4 lat           12352     1      25.3    8.53     25   9.93    8.1   57    48.9
#>  5 long          12352     1     -63.1   18.6     -63  21.6  -127.   -12.8 114. 
#>  6 category       5100     0.413   1.90   1.15      1   0       1      5     4  
#>  7 wind          12352     1      62.6   25.0      55  22.2    35    165   130  
#>  8 pressure      12352     1     987.    20.0     993  16.3   882   1016   134  
#>  9 tropicalst…    6460     0.523 183.   115.      160 104.     10    870   860  
#> 10 hurricane_…    6460     0.523  23.7   38.4       0   0       0    300   300  
#> # ℹ 2 more variables: skew <dbl>, kurt <dbl>

# with subset of default stats
cb_summarize_numeric(
  cb_storms,
  stats = list(mean = mean, SD = sd)
)
#> # A tibble: 10 × 5
#>    name                         valid_n valid_pct   mean     SD
#>    <chr>                          <int>     <dbl>  <dbl>  <dbl>
#>  1 month                          12352     1       8.80   1.22
#>  2 day                            12352     1      15.7    8.98
#>  3 hour                           12352     1       9.14   6.72
#>  4 lat                            12352     1      25.3    8.53
#>  5 long                           12352     1     -63.1   18.6 
#>  6 category                        5100     0.413   1.90   1.15
#>  7 wind                           12352     1      62.6   25.0 
#>  8 pressure                       12352     1     987.    20.0 
#>  9 tropicalstorm_force_diameter    6460     0.523 183.   115.  
#> 10 hurricane_force_diameter        6460     0.523  23.7   38.4 

# grouped summary
cb_summarize_numeric(
  cb_storms,
  stats = list(mean = mean, SD = sd),
  group_by = status
)
#> # A tibble: 20 × 6
#>    status         name                         valid_n valid_pct   mean      SD
#>    <fct>          <chr>                          <int>     <dbl>  <dbl>   <dbl>
#>  1 hurricane      month                           5100     1       8.96   0.964
#>  2 tropical storm month                           7252     1       8.68   1.36 
#>  3 hurricane      day                             5100     1      15.8    8.99 
#>  4 tropical storm day                             7252     1      15.6    8.97 
#>  5 hurricane      hour                            5100     1       9.16   6.75 
#>  6 tropical storm hour                            7252     1       9.12   6.71 
#>  7 hurricane      lat                             5100     1      26.4    8.10 
#>  8 tropical storm lat                             7252     1      24.5    8.73 
#>  9 hurricane      long                            5100     1     -63.9   16.7  
#> 10 tropical storm long                            7252     1     -62.6   19.8  
#> 11 hurricane      category                        5100     1       1.90   1.15 
#> 12 tropical storm category                           0     0     NaN     NA    
#> 13 hurricane      wind                            5100     1      86.7   20.7  
#> 14 tropical storm wind                            7252     1      45.6    8.25 
#> 15 hurricane      pressure                        5100     1     969.    18.7  
#> 16 tropical storm pressure                        7252     1     999.     6.90 
#> 17 hurricane      tropicalstorm_force_diameter    2467     0.484 253.   116.   
#> 18 tropical storm tropicalstorm_force_diameter    3993     0.551 139.    89.6  
#> 19 hurricane      hurricane_force_diameter        2467     0.484  62.1   38.4  
#> 20 tropical storm hurricane_force_diameter        3993     0.551   0      0    

# with custom stats
cb_summarize_numeric(
  cb_storms,
  stats = list(
    median = median,
    q25 = \(x) quantile(x, 0.25, na.rm = TRUE),
    q75 = \(x) quantile(x, 0.75, na.rm = TRUE),
    IQR = IQR
  )
)
#> # A tibble: 10 × 7
#>    name                         valid_n valid_pct median   q25    q75   IQR
#>    <chr>                          <int>     <dbl>  <dbl> <dbl>  <dbl> <dbl>
#>  1 month                          12352     1          9   8      9     1  
#>  2 day                            12352     1         16   8     23    15  
#>  3 hour                           12352     1         12   6     18    12  
#>  4 lat                            12352     1         25  18.1   31.5  13.4
#>  5 long                           12352     1        -63 -77.9  -48.7  29.2
#>  6 category                        5100     0.413      1   1      3     2  
#>  7 wind                           12352     1         55  45     75    30  
#>  8 pressure                       12352     1        993 977   1002    25  
#>  9 tropicalstorm_force_diameter    6460     0.523    160 100    240   140  
#> 10 hurricane_force_diameter        6460     0.523      0   0     40    40  
```
