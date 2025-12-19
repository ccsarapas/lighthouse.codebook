# lighthouse.codebook 0.0.2

## New functionality

* New function `cb_summarize_text()` returns a summary of text variables including
  unique values, frequencies for the most common values, and missing value information.
  A tab including this information is now included in workbooks created by `cb_write()`.

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

* In `cb_write()`, changed `detail_missing` argument from logical to options `"if_any_user_missing"`, 
  `"yes"`, and `"no"`.

* `cb_summarize_numeric()`, `cb_summarize_categorical()`, and `cb_summarize_text()`
  now return `NULL` when there are no variables of the relevant type, with an optional
  warning controlled by the `.warn_if_none` argument.

## Interface changes

* In `cb_create()` and variants, changed `.rmv_html` and `.rmv_line_breaks` arguments
  to logical rather than tidyselect.
