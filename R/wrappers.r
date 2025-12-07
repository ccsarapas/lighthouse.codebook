

cb_create_spss <- function(data,
                           .user_missing = NULL,
                           # these would require a different implementation -- omitted for now
                           #   .rmv_html = !name,
                           #   .rmv_line_breaks = !name,
                           .separate_missings = c("if_any", "yes", "no"),
                           .user_missing_conflict = c("val_label", "missing_label")) {
  .separate_missings <- match.arg(.separate_missings)
  .user_missing_conflict <- match.arg(.user_missing_conflict)
  .user_missing_conflict <- sub("val_label", "metadata", .user_missing_conflict)
  data |>
    cb_init() |>
    cb_add_label_col_spss() |> 
    cb_update_labels_spss(
      user_missing = .user_missing,
      conflict = .user_missing_conflict
    ) |>
    cb_zap_data_spss() |>
    cb_add_dims() |>
    cb_add_val_labels_col(separate_missings = .separate_missings) |>
    cb_add_type_col() |>
    cb_add_missing_col()
}




#' Generate a codebook object
#'
#' @description
#' `cb_create()` builds an object of class `"li_codebook"` from a dataset and optional
#' metadata. The resulting object can be used to write an Excel workbook with variable
#' and data summaries (using [`cb_write()`]), extract processed data ([`cb_get_data()`]),
#' or generate dataset summaries ([`cb_summarize_numeric()`] and [`cb_summarize_categorical()`]).
#'
#' @param data A data frame exported or retrieved from REDCap.
#' @param metadata A data frame containing metadata, such as variable labels and value 
#' labels.
#' @param ... Additional columns from `metadata` to preserve in the final codebook.
#'   New names can be assigned by passing named arguments. Columns for variable
#'   name, form, variable label, and value labels are included by default.
#' @param .name,.var_label,.val_labels Columns in `metadata` containing variable
#'   name, variable label, and value labels, respectively. If `metadata` is provided, 
#'   `.name` must be specified. `.var_label` and `.val_labels` may be set to `NULL` 
#'   to omit.
#' @param .user_missing A formula or list of formulas specifying user missing values.
#'   Formulas should specify variables on the left-hand side (as variable names
#'   or [tidyselect][dplyr_tidy_select] expressions), and missing values on the
#'   right-hand side. See "Specifying user missing values" below for examples.
#' @param .val_labs_sep1,.val_labs_sep2 Regex patterns separating value labels
#'   in `metadata`. `.val_labs_sep1` separates values from labels, and `.val_labs_sep2` 
#'   separates value/label pairs. e.g., if value labels are in format `"1, First label|2, Second label"`,
#'   set `.val_labs_sep1` to `","` and `.val_labs_sep2` to `"\\|"`.
#' @param .rmv_html <[`tidy-select`][dplyr_tidy_select]> Codebook columns from which
#'   HTML tags should be removed.
#' @param .rmv_line_breaks <[`tidy-select`][dplyr_tidy_select]> Codebook columns
#'   from which line breaks should be removed.
#' @param .separate_missings Include value labels for user missing values in a separate
#'   column? The default, `"if_any"`, adds the column only if user missings are
#'   specified for at least one variable.
#' @param .user_missing_conflict If different labels for a value are provided in
#'   metadata and user missings, which should be used?
#'
#' @return
#' An `"li_codebook"` object, consisting of (1) a tibble summarizing the passed
#' dataset and (2) attributes containing the passed dataset (in several formats)
#' and additional metadata. Specifically:
#' - A tibble with columns:
#'     - `name`: variable name
#'     - `type`: variable type
#'     - `label`: variable label
#'     - `value_labels`: value labels
#'     - `user_missing`: optional column, depending on value of `.separate_missings`,
#'        with value labels for user missing values
#'     - `missing`: proportion missing
#'     - additional columns if specified in `...`
#' - Attributes:
#'     - Transformed versions of the passed dataset. See [`cb_get_data()`].
#'     - Lookup tables and other metadata used internally: `"user_missing"`, `"vals_by_label"`,
#'       `"labs_by_value"`, `"miss_propagate"`, `"factors"`, `"n_obs"`, `"n_vars"`
#'
#' @section Specifying user missing values:
#' User missing values are defined by passing a formula or list of formulas to the
#' `.user_missing` argument. Formulas should specify variables on the left-hand
#' side and user missing values for those variables on the right-hand side:
#' \preformatted{
#' cb <- cb_create_redcap(data, metadata, .user_missing = var1 ~ 99)
#' }
#' The same user missings can be applied to multiple variables using
#'   [tidyselect][dplyr_tidy_select] expressions:
#' \preformatted{
#' # for variables `var1` through `var5`
#' .user_missing = var1:var5 ~ 99
#'
#' # for all numeric variables, plus `var6` and `var7`
#' .user_missing = c(where(is.numeric), var6, var7) ~ c(-9, -8, -7)
#' }
#' Different user missings can be applied to different variables using a list of
#' formulas:
#' \preformatted{
#' .user_missing = list(
#'   starts_with("status") ~ c(98, 99),
#'   var7:var10 ~ 97
#' )
#' }
#' User missing values may optionally be named to set value labels:
#' \preformatted{
#' .user_missing = where(is.numeric) ~
#'   c(Declined = -97, "Don't know" = -98, "Not applicable" = -99)
#' }
#' If labels set in `.user_missing` conflict with those in `metadata`, `.user_missing_conflict`
#' controls which labels are used.
#' 
#' @examples
#' diamonds2 <- ggplot2::diamonds |>
#'   transform(
#'     carat_group = as.integer(cut(carat, breaks = 3, labels = 1:3)),
#'     price_group = as.integer(cut(
#'       price,
#'       breaks = c(0, 500, 1000, 2000, 5000, 10000, Inf),
#'       labels = 1:6,
#'       right = FALSE
#'     ))
#'   )
#' 
#' # basic codebook
#' cb_create(diamonds2)
#' 
#' # convert variables to factor to treat as categorical
#' diamonds2 |>
#'   transform(
#'     carat_group = factor(carat_group),
#'     price_group = factor(price_group)
#'   ) |> 
#'   cb_create()
#' 
#' # provide metadata for variable and value labels
#' diamonds_meta <- data.frame(
#'   name = names(diamonds2),
#'   label = c(
#'     # from ?ggplot2::diamonds
#'     "price in US dollars ($326–$18,823)",
#'     "weight of the diamond (0.2–5.01)",
#'     "quality of the cut (Fair, Good, Very Good, Premium, Ideal)",
#'     "diamond colour, from D (best) to J (worst)",
#'     "a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))",
#'     "length in mm (0–10.74)",
#'     "width in mm (0–58.9)",
#'     "depth in mm (0–31.8)",
#'     "total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)",
#'     "width of top of diamond relative to widest point (43–95)",
#'     "diamond carat (3 groups)",
#'     "diamond price (6 groups)"
#'   ),
#'   val_labels = c(
#'     rep(NA, 10),
#'     "1 = small; 2 = medium; 3 = large",
#'     "1 = <$500; 2 = $500-$999; 3 = $1,000-$1,999; 4 = $2,000-$4,999; 5 = $5,000-$9,999; 6 = $10,000+"
#'   )
#' )
#' 
#' cb_create(
#'   diamonds2, diamonds_meta,
#'   .val_labs_sep1 = " = ", .val_labs_sep2 = "; "
#' )
#' @export
cb_create <- function(data,
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
                      .user_missing_conflict = c("metadata", "missing_label")) {
  .separate_missings <- match.arg(.separate_missings)
  .user_missing_conflict <- match.arg(.user_missing_conflict)
  data |>
    cb_init(
      metadata,
      meta_var_name = {{ .name }}, meta_var_label = {{ .var_label }},
      meta_val_labels = {{ .val_labels }}, ...
    ) |>
    cb_clean_fields(
      rmv_html = {{ .rmv_html }},
      rmv_line_breaks = {{ .rmv_line_breaks }}
    ) |>
    cb_user_missings(user_missing = .user_missing) |>
    cb_add_lookups(sep1 = .val_labs_sep1, sep2 = .val_labs_sep2) |>
    cb_label_data(conflict = .user_missing_conflict) |>
    cb_zap_data() |>
    cb_add_dims() |>
    cb_add_val_labels_col(separate_missings = .separate_missings) |>
    cb_add_type_col() |>
    cb_add_missing_col()
}

#' Generate a codebook object from REDCap data
#'
#' @description
#' `cb_create_redcap()` builds an object of class `"li_codebook"` from a dataset and
#' corresponding codebook exported from REDCap. The resulting object can be used
#' to write an Excel workbook with variable and data summaries (using [`cb_write()`]),
#' extract processed data ([`cb_get_data()`]), or generate dataset summaries ([`cb_summarize_numeric()`]
#' and [`cb_summarize_categorical()`]).
#'
#' This variant of [`cb_create()`] includes functionality specific to REDCap data
#' and metadata, including:
#' - Defaults for typical REDCap metadata column names
#' - Includes form field by default
#' - Unpacking, labelling, and optional missing propagation for checkbox data
#' - Optional coercion for character variables marked as "integer" in `metedata$text_validation_type_or_show_slider_number`
#' 
#' @inheritParams cb_create
#' @param data A data frame exported or retrieved from REDCap.
#' @param metadata A data frame containing the REDCap codebook associated with `data`.
#' @param ... Additional columns from `metadata` to preserve in the final codebook.
#'   New names can be assigned by passing named arguments. Columns for variable
#'   name, form, variable label, and value labels are included by default.
#' @param .name,.var_label,.val_labels Columns in `metadata` containing variable
#'   name, variable label, and value labels, respectively.
#' @param .form Column in `metadata` containing form names. (Set to `NULL` to omit.)
#' @param .user_missing A formula or list of formulas specifying user missing values.
#'   Formulas should specify variables on the left-hand side (as variable names
#'   or [tidyselect][dplyr_tidy_select] expressions), and missing values on the
#'   right-hand side. See "Specifying user missing values" in [`cb_create()`] documentation 
#'   for examples.
#' @param .coerce_integers Should variables listed as "integer" in `metedata$text_validation_type_or_show_slider_number` 
#'   be coerced to integer?
#' @param .checkbox_resp_values Should checkbox values use labels in `metadata` 
#'   (`TRUE`) or "Yes" / "No" (`FALSE`)? See "Checkbox data handling" below.
#' @param .propagate_checkbox_missings Should user missing values in a checkbox 
#'   group be propagated across all variables in the group? See "Checkbox data handling" 
#'   below.
#'
#' @return
#' An `"li_codebook"` object, consisting of (1) a tibble summarizing the passed
#' dataset and (2) attributes containing the passed dataset (in several formats)
#' and additional metadata. Specifically:
#' - A tibble with columns:
#'     - `name`: variable name
#'     - `form`: form name
#'     - `type`: variable type
#'     - `label`: variable label
#'     - `value_labels`: value labels
#'     - `user_missing`: optional column, depending on value of `.separate_missings`,
#'        with value labels for user missing values
#'     - `missing`: proportion missing
#'     - additional columns if specified in `...`
#' - Attributes:
#'     - Transformed versions of the passed dataset. See [`cb_get_data()`].
#'     - Lookup tables and other metadata used internally: `"user_missing"`, `"vals_by_label"`,
#'       `"labs_by_value"`, `"miss_propagate"`, `"factors"`, `"n_obs"`, `"n_vars"`
#'
#' @section Checkbox data handling:
#' ## Value labels
#' Data from REDCap checkboxes yields one variable in the dataset for each response
#' option. These will be labelled generically with `"Yes"` or `"No"`, unless `.checkbox_resp_values`
#' is `TRUE`, in which case response-specific labels from `metadata` will be used.
#' For example, if a checkbox group has options "In the past year," "More than a
#' year ago," and "Never," corresponding to variables `chk_var1___0`, `chk_var1___1`,
#' and `chk_var1___2`: if `.checkbox_resp_values` is `FALSE`, all of these will
#' have values:
#'   - `chk_var1___0`, `chk_var1___1`, `chk_var1___2`: 0 = "No"; 1 =  "Yes". 
#' 
#' If `.checkbox_resp_values` is `TRUE`, each variable will have unique labels:
#'   - `chk_var1___0`: 0 = "Not selected," 1 = "In the past year"
#'   - `chk_var1___1`: 0 = "Not selected," 1 = "More than a year ago"
#'   - `chk_var1___2`: 0 = "Not selected," 0 = "Never"
#'
#' ## Missing value propagation
#' If `.propagate_checkbox_missings` is `TRUE`, missing values in a checkbox group
#' variable will be propagated to all variables in the group. For example, given
#' a checkbox group with options "Pregnant," "Not pregnant," and "Not applicable,"
#' corresponding to variables `chk_preg_0___0`, `chk_preg_0___1`, and `chk_preg_0____9`,
#' and assuming that `-9` is specified as a user missing value. If `.propagate_checkbox_missings`
#' is `TRUE`, `chk_preg_0___0` and `chk_preg_0___1` will be set to `-9` if `chk_preg_0____9`
#' is `1`. Otherwise, these columns will remain as `0` where `chk_preg_0____9` is `1`.
#'
#' @export
cb_create_redcap <- function(data,
                         metadata,
                         ...,
                         .name = field_name,
                         .var_label = field_label,
                         .val_labels = select_choices_or_calculations,
                         .form = form_name,
                         .user_missing = NULL,
                         .val_labs_sep1 = ", ",
                         .val_labs_sep2 = "\\|",
                         .rmv_html = !name,
                         .rmv_line_breaks = !name,
                         .coerce_integers = TRUE,
                         .checkbox_resp_values = FALSE,
                         .propagate_checkbox_missings = TRUE,
                         .separate_missings = c("if_any", "yes", "no"),
                         .user_missing_conflict = c("metadata", "missing_label")) {
  .separate_missings <- match.arg(.separate_missings)
  .user_missing_conflict <- match.arg(.user_missing_conflict)
  meta <- meta_expand_checkboxes_rc(metadata, data)
  cb <- data |>
    cb_init(
      meta,
      meta_var_name = {{ .name }}, meta_var_label = {{ .var_label }},
      meta_val_labels = {{ .val_labels }}, form = {{ .form }}, ...,
      ..rc_type = field_type,
      ..rc_validate_type = text_validation_type_or_show_slider_number,
    )
  if (.coerce_integers) cb <- cb_coerce_integers_rc(cb)
  cb$..rc_validate_type <- NULL
  cb <- cb |>
    cb_clean_fields(
      rmv_html = {{ .rmv_html }},
      rmv_line_breaks = {{ .rmv_line_breaks }}
    ) |>
    cb_user_missings(user_missing = .user_missing) |>
    cb_add_lookups(sep1 = .val_labs_sep1, sep2 = .val_labs_sep2) |>
    cb_relabel_checkboxes_rc(use_resp_values = .checkbox_resp_values)
  if ("form" %in% names(cb)) cb <- cb_complete_label_rc(cb)
  if (.propagate_checkbox_missings) {
    cb <- cb_propagate_user_missing_checkboxes_rc(cb)
  }
  cb |>
    cb_label_data(conflict = .user_missing_conflict) |>
    cb_zap_data() |>
    cb_add_dims() |>
    cb_add_val_labels_col(separate_missings = .separate_missings) |>
    cb_add_type_col() |>
    cb_add_missing_col() |>
    dplyr::relocate(tidyselect::any_of("form"), type, .after = name)
}


#' Extract data from a codebook object
#'
#' Codebook objects created by [`cb_create()`] and friends contain several transformed
#' versions of the originally passed dataset. These can be extracted using `cb_get_data()`.
#'
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param format Format of the returned data; see below for details.
#'
#' @return
#' A tibble with variables formatted based on the `format` argument.
#' - For `format = "values"`, all variables retain the same values as the original
#'   dataset, including values for user missings. The data may reflect transformations
#'   made by variants of [`cb_create()`] -- e.g., for [`cb_create_redcap()`], integer coercion 
#'   and propagation of user missings across checkbox variables.
#' - For `"haven"`, value labels and user missings are encoded using class 
#'   [`"haven_labelled"`][haven::labelled]`
#' - For `"factors"`, all variables with value labels are converted to factors, 
#'   and all user missings are converted to `NA`.
#' 
#' @export
cb_get_data <- function(cb, format = c("factors", "haven", "values")) {
  check_codebook(cb)
  switch(match.arg(format),
    factors = attr(cb, "data_zapped"),
    haven = attr(cb, "data_labelled"),
    values = attr(cb, "data")
  )
}

#' Write codebook and data summaries to an Excel workbook
#' 
#' `cb_write()` writes an Excel workbook to disk with tabs including a codebook; 
#' summary statistics for numeric variables; frequencies for categorical variables; 
#' and optional grouped data summaries. For data summaires, variables with value 
#' labels, factors (including ordered factors), and logical variables are treated 
#' as categorical, while numeric and integer variables are treated as numeric.
#' 
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param file Path to write to.
#' @param dataset_name Name of the dataset to display in workbook headers.
#' @param incl_date,incl_dims Should the date and/or dataset dimensions be included 
#'   in the Overview tab header?
#' @param group_by <[`tidy-select`][dplyr_tidy_select]> Column or columns to group
#'   by. If specified, additional numeric and categorical summary tabs will be included
#'   with decked heads for specified groups. 
#' @param detail_missing Include detailed missing value information on categorical 
#'   summary tab?
#' @param overwrite Overwrite existing file?
#'
#' @return Invisibly returns the path to the written Excel file.
#' 
#' @export
cb_write <- function(cb, 
                     file, 
                     dataset_name = NULL,
                     incl_date = TRUE,
                     incl_dims = TRUE,
                     group_by = NULL,
                     detail_missing = TRUE,
                     overwrite = TRUE) {
  check_codebook(cb)
  summaries <- list(
    num = cb_summarize_numeric(cb),
    cat = cb_summarize_categorical(cb, detail_missing = detail_missing)
  )
  group_by <- rlang::enquo(group_by)
  if (!rlang::quo_is_null(group_by)) {
    if (detail_missing) {
      cli::cli_inform(c(
        "i" = paste0(
          "Detailed missing value information is not currently supported for ", 
          "grouped summaries, so will be included only for ungrouped summaries."
        )
      ))
    }
    summaries$grouped <- list(
      num = cb_summarize_numeric(cb, group_by = !!group_by),
      cat = cb_summarize_categorical(cb, group_by = !!group_by)
    )
  }
  cb_write_codebook(
    cb, summaries,
    file = file, dataset_name = dataset_name, incl_date = incl_date, 
    incl_dims = incl_dims, overwrite = overwrite
  )
}



