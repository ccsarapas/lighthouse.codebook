
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lighthouse.codebook

The lighthouse.codebook package includes tools to summarize a dataset
into a formatted Excel workbook, including a data dictionary and
summaries. It incorporates external metadata (such as variable labels,
value labels, and user missing / non-response codes), with convenience
functions for using metadata from SPSS and REDCap datasets. Codebooks
can be customized in a number of ways, including options for grouped
summaries.

## Installation

You can install lighthouse.codebook by running:

``` r
# install.packages("remotes")
remotes::install_github("ccsarapas/lighthouse.codebook")
```

## Creating a codebook

<!-- 1485 x 525 -->
<!-- 1275 x 525 -->
<!-- 1405 x 570 -->

Creating a codebook involves two general steps:

1.  Create a a “codebook” object in R from a data frame (and,
    optionally, metadata), using `cb_create()` or a specialized variant
    (such as `cb_create_spss()` or `cb_create_redcap()`).

2.  Write the codebook to disk using `cb_write()`.

Thus, in the simplest case, to create a codebook and write it to disk:.

``` r
gain_q4 |> cb_create() |> cb_write("q4.xlsx")
```

`cb_create()` and `cb_write()` include a number of options for
controlling how data is interpreted, summarized, and presented.
<!-- - The "Creating Codebooks" vignette covers options for controlling how data and 
metadata are _interpreted,_ such as by applying value labels, specifying user missing 
or nonresponse codes, and taking advantage of specialized metadata (e.g., from SPSS 
or REDCap data).
- The "Writing Codebooks" vignette covers how data is _summarized and presented_ 
in the codebook written to disk, including options for grouped summaries and missing 
data. -->

## Codebook contents

The codebook written to disk will include an Overview tab listing all
variables in the dataset; Summary tabs for numeric, categorical, and
text variables; and, if grouping variables are specified, Grouped
Summary tabs for numeric and categorical variables.

The Overview tab includes one row for each variable in the dataset, with
information on variable types, labels, values, and missingness. By
default, each variable is hyperlinked to its location on the relevant
summary tab.

<img src="man/figures/README-overview.png" width="100%" style="display: block; margin: auto;" />

The numeric summary tab includes descriptive statistics for all numeric
variables in the dataset:

<img src="man/figures/README-numeric.png" width="100%" style="display: block; margin: auto;" />

The categorical summary tab includes frequencies for all categorical
variables, optionally with separate rows for user missing values:

<img src="man/figures/README-categorical.png" width="100%" style="display: block; margin: auto;" />

The text summary tab includes frequencies for the most common values for
all text variables in the dataset.

<img src="man/figures/README-text.png" width="100%" style="display: block; margin: auto;" />

## SPSS extension

Functionality from this package is also available as an SPSS extension
command [here](https://github.com/ccsarapas/lighthouse.codebook.spss).
