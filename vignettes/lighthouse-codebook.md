---
title: "Introduction to lighthouse.codebook"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to lighthouse.codebook}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  eval = FALSE,
  comment = "#"
)
```
### Workflow overview

Creating a codebook using the lighthouse.codebook package generally involves two steps. First, create a "codebook" object from a data frame (and, optionally, metadata) using `cb_create()` or a specialized variant (such as `cb_create_spss()` or `cb_create_redcap()`). Second, write the codebook to disk using `cb_write()`.

In many cases, this can be done in a single pipeline.
```r
library(lighthouse.codebook)

# using example dataset included with package
gain_q4 |> cb_create() |> cb_write("q4.xlsx")
```


A codebook written to disk will include an overview tab as well as summary tabs for numeric, categorical, and text variables. The overview tab lists all variables with their labels and missing value information. By default, each variable is hyperlinked to its location on a Summary tab.
```{r eval = TRUE}
knitr::include_graphics("img/overview.png")
```

The numeric summary tab includes descriptive statistics for all numeric variables in the dataset:
[image]

The categorical summary tab includes frequencies for all categorical variables in the dataset:
[image]

The text summary tab includes the number of unique values, and frequencies for the most common values, for all text variables in the dataset. (The number of values for which frequencies are shown can be specified using the `n_text_vals` argument to `cb_write()`; by default, the 5 most common values are shown.)
[image]

The codebook object can also be used to create other objects in R, such 
as transformed data (see `cb_get_data()`) and summaries (see `cb_summarize_numeric()`, 
`cb_summarize_categorical()`, `cb_summarize_text()`).
```r
cb <- cb_create(gain_q4)

# data with SPSS-style variable and value labels
q4_haven <- cb_get_data(cb, format = "haven")

# descriptives for numeric variables
q4_summary_num <- cb_summarize_numeric(cb)
```

### Creating codebooks

1. In simplest case, use `cb_create()` with "basic" dataframe.
   - \*\*\* make a simpler example dataframe for this
   - simple eg code
2. Other options - probably show *very* briefly, then link to separate vignette for each:
   - Use `cb_create()` with user-supplied metadata (link to vignette).
   - Use `cb_create_redcap()` with data and metadata exported from REDCap (link to vignette).
   - Use `cb_create_spss()` with SPSS data imported using haven (link to vignette).

### Variable types
Data summaries are produced for "numeric," "categorical," and "text" variables. For a given variable `x`,
- `x` is treated as categorical if (1) it is a factor, ordered factor, or logical vector, _or_ (2) it has associated value labels other than missing value codes (specified in metadata or, for SPSS data, in a `"haven_labelled"` vector).
- `x` is treated as numeric if (1) it is numeric (i.e., `is.numeric(x)` is `TRUE`) _and_ (2) it has no associated value labels other than missing value codes.
- `x` is treated as text if (1) it is a character vector _and_ (2) it has no associated value labels other than missing value codes.

Thus, you can change the way a variable is summarized by changing its class. For instance, to get complete frequencies for a numeric or character variable, convert it to factor; or to get only the top frequencies for a factor with many levels, convert it to character.

Variables of other classes, such as dates, datetimes, and lists, are not currently included on summary tabs. (Summaries for dates and datetimes are planned for a future release).

### Missing values

User missing values (also known as nonresponse codes, reserve codes, or special values) can be specified in the call to `cb_create()` with the `.user_missing` argument. Missing codes are specified using a formula or list of formulas; formulas should have variable(s) on the left-hand side (as a [tidyselect expression](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)) and user missing values on the right-hand side. 
** ...adapt subsection on missing values from `cb_create()`. **
If the left-hand side is empty, the specified values will be treated as user missing across all compatible variables in the dataset.
```r
cb <- cb_create(dat, .user_missing = ~ -99)
```

User missing values are dropped for computation of summary statistics for numeric variables. By default, user missing values are individually tabulated on ungrouped categorical and text summary tabs:
```r
missing_codes <- c(
  "Not Asked" = -3, "Missing" = -4, "Confidential" = -6, "Refused" = -7, 
  "Don't Know" = -8, "Legitimate Skip" = -9
)

dat |>
  cb_create(.user_missing = ~ missing_codes) |>
  cb_write("cb.xlsx")
```
[image]
...but can be collapsed with system missing values into a single "(Missing)" row by setting `detail_missing` to `FALSE`:
```r
dat |>
  cb_create(.user_missing = ~ missing_codes) |>
  cb_write("cb.xlsx", detail_missing = FALSE)
```
However, user missing values are always collapsed (as though `detail_missing = FALSE`) on _grouped_ summary tabs. 
```
### Grouped summaries
Grouped summaries can be generated by specifying one or more grouping variables in the `group_by` argument to `cb_write()`. The workbook will contain both ungrouped summary tabs and tabs grouped by the specified variables. By default, grouping is done in columns, using decked heads when multiple grouping variables are specified.
```r
dat |>
  cb_create() |>
  cb_write(group_by = c(XRA, XOBS))
```
[image]
For numeric variables, grouping can instead be done in rows by passing some or all grouping variables to `group_rows_numeric`:
```r
dat |>
  cb_create() |>
  cb_write(group_by = c(XRA, XOBS), group_rows_numeric = XOBS)
```
[image]

### Handling long / repetitive variable labels

- Adapt section on `.split_var_labels` from NEWS.

...




## `cb_create()` vignette on 
#### User-supplied metadata
   - by default, `cb_create()` expects columns `"name"` with variable names, `"label"` with variable labels, and `"val_labels"` with value labels.
   - however, only `"name"` is required.
   - alternate names in `metadata` can be specified using `.name`, `.var_label`, and `.val_labels` arg. (or you can just change the names in `metadata`.)
   - value labels should be a single string per variable; specify separators
   - can keep additional columns from metadata by passing to `...`