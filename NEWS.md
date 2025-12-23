# lighthouse.codebook 0.1.0

* New function `cb_summarize_text()` returns a summary of text variables including
  unique values, frequencies for the most common values, and missing value information.
  A tab including this information is now included in workbooks created by `cb_write()`.
  
* In workbooks produced by `cb_write()`, variable names on the Overview sheet are 
  now hyperlinked to corresponding rows on summary tabs and vice versa. This can 
  be turned off by setting `hyperlinks` to `FALSE`. 

* The `.split_var_labels` argument to `cb_create()` and variants splits variable 
  labels with a common prefix into separate columns. For example, given a set of 
  variable labels that all begin with `"Select all that apply: "`, the default behavior 
  is: 
  
  | Name | Label |
  | ---- | ----- |
  | colors1 | Select all that apply: Red |
  | colors2 | Select all that apply: Green |
  | colors3 | Select all that apply: Blue |
  | colors4 | Select all that apply: Orange |
  
  ...but specifying `.split_var_labels = colors1:colors4` yields:
  
  | Name | Label Stem | Label |
  | ---- | ---------- | ----- |
  | colors1 | Select all that apply: | Red |
  | colors2 | Select all that apply: | Green |
  | colors3 | Select all that apply: | Blue |
  | colors4 | Select all that apply: | Orange |

  Multiple sets of variables with common prefixes can be specified by passing a 
  list of tidyselect expressions.

* `cb_create()` and variants now return more general type information (e.g., `"date-time`"
  rather than `"POSIXct, POSIXt"`) when `.include_types` is `TRUE` (the default). 
  Detailed class information can be requested by setting `.include_r_classes` to `TRUE`.

* _n_ for each subgroup are now included in decked headers for grouped summaries.

* In `cb_write()`, changed `detail_missing` argument from logical to options `"if_any_user_missing"`, 
  `"yes"`, and `"no"`.

* `cb_summarize_numeric()`, `cb_summarize_categorical()`, and `cb_summarize_text()`
  now return `NULL` when there are no variables of the relevant type, with an optional
  warning controlled by the `.warn_if_none` argument.

* Categorical, numeric, and/or text summary tabs are now omitted when there are 
  no variables of the relevant type.

* In `cb_create()` and variants, changed `.rmv_html` and `.rmv_line_breaks` arguments
  to logical rather than tidyselect.
