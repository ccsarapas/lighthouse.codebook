#' Generate a codebook object from REDCap data
#' 
#' `cb_create()` creates an object of class `"li_codebook"` from a dataset and codebook 
#' exported from REDCap. The codebook object can be used to write an Excel workbook 
#' with variable and data summaries (using [`cb_write()`]), to extract processed 
#' data ([`cb_get_data()`]), or to generate dataset summaries (using [`cb_summaries()`]).
#'
#' @param data A data frame; data exported or retrieved from REDCap.
#' @param metadata A data frame; a codebook exported or retrieved from REDCap.
#' @param ... Additional columns from `metadata` to preserve in the final codebook. 
#'   (Columns for variable name, form, variable label, and value labels will be 
#'   included by default.)
#' @param .user_missing A formula or list of formulas specifying user missing values. 
#'   Formulas should specify variables on the left-hand side (as variable names 
#'   or tidyselect expressions), and missing values on the right-hand side. See Details.
#' @param .val_labs_sep1, .val_labs_sep2 Regex patterns separating value labels 
#'   in `metadata`. e.g., if value labels are in format `"1, First label|2, Second label"`, 
#'   set `.val_labs_sep1` to `","` and `.val_labs_sep2` to `"\\|"`.
#' @param .rmv_html <[`tidy-select`][dplyr_tidy_select]> HTML tags will be removed 
#'   from these codebook columns.
#' @param .rmv_line_breaks <[`tidy-select`][dplyr_tidy_select]> Line breaks will 
#'   be removed from these codebook columns.
#' @param .checkbox_resp_values A logical indicating whether checkbox values should 
#'   be labelled using labels in `metadata` (`TRUE`) or as "Yes" / "No" (`FALSE`).
#' @param .separate_missings Include value labels for user missing values in a seprate 
#'   column? The default, `"if_any"`, will include a separate column only if user 
#'   missings are specified for at least one variable. 
#' @param .user_missing_conflict If different labels for a value are provided in 
#'   metadata and user missings, which should be used?
#' 
#' @return
#' An `"li_codebook"` object, which consists of (1) a dataframe summarizing the 
#' passed dataset and (2) attributes containing the passed dataset (in several
#' formats) and additional metadata. Specifically:
#' - A data frame with columns:
#'     - `name`: variable name
#'     - `form`: form name
#'     - `type`: variable type
#'     - `label`: variable label
#'     - `value_labels`: value labels
#'     - `user_missing`: optional column, depending on value of `.separate_missings`, with value labels for user missing values
#'     - `missing`: proportion missing
#'     - additional columns if specified in `...`
#' - Attributes:
#'     - `"data"`: unmodified passed dataset
#'     - `"data_labelled"`: passed dataset after transformations, with value labels and user missings encoded using class [`"haven_labelled"`][haven::labelled]`
#'     - `"data_zapped"`: passed dataset after transformations, with categorical variables converted to factors
#'     - `"user_missing"`, `"vals_by_label"`, `"labs_by_value"`, `"miss_propagate"`, `"factors"`, `"n_obs"`, `"n_vars"`: lookup tables and other metadata used internally
#' 
#' @details 
#' TODO:
#' - elaborate on how to specify `.user_missing`, with examples
#' - describe transformations applied to data (specific to REDCap data) 
#'     - coercion to integer
#'     - NA propagation for checkboxes
#' - etc
#' 
#' @examples 
#' TODO:
#' - will need example dataset to illustrate this and other functions...
#' 
#' @export
cb_create_rc <- function(data, 
                      metadata, 
                      ...,
                      .user_missing = NULL, 
                      .val_labs_sep1 = ", ", 
                      .val_labs_sep2 = "\\|",
                      .rmv_html = !name, 
                      .rmv_line_breaks = !name,
                      .checkbox_resp_values = FALSE,
                      .separate_missings = c("if_any", "yes", "no"),
                      .user_missing_conflict = c("metadata", "missing_label")) {
  .separate_missings <- match.arg(.separate_missings)
  .user_missing_conflict <- match.arg(.user_missing_conflict)
  meta <- meta_expand_checkboxes_rc(metadata, data)
  data |>
    cb_init(
      meta,
      meta_var_name = field_name, meta_var_label = field_label,
      meta_val_labels = select_choices_or_calculations, form = form_name, ...,
      ..rc_type = field_type,
      ..rc_validate_type = text_validation_type_or_show_slider_number,
    ) |>
    cb_coerce_integers_rc() |>
    cb_clean_fields(
      rmv_html = {{ .rmv_html }}, 
      rmv_line_breaks = {{ .rmv_line_breaks }}
    ) |>
    cb_user_missings(user_missing = .user_missing) |>
    cb_add_lookups(sep1 = .val_labs_sep1, sep2 = .val_labs_sep2) |>
    cb_relabel_checkboxes_rc(use_resp_values = .checkbox_resp_values) |>
    cb_complete_label_rc() |>
    cb_propagate_user_missing_checkboxes_rc() |>
    cb_label_data(conflict = .user_missing_conflict) |>
    cb_zap_data() |>
    cb_add_dims() |>
    cb_add_val_labels(separate_missings = .separate_missings) |>
    cb_add_types() |>
    cb_add_missing() |> 
    dplyr::relocate(form, type, .after = name)
}

#' Summarize all numeric and categorical variables from a codebook object
#' 
cb_summaries <- function(cb, group_by = NULL, detail_missing = missing(group_by)) {
    list(
      num = cb_summarize_numeric(cb, group_by = {{ group_by }}),
      cat = cb_summarize_categorical(
        cb, group_by = {{ group_by }}, detail_missing = detail_missing
      )
    )
}

#' Extract data from a codebook object
#' 
cb_get_data <- function(cb, format = c("zapped", "labelled", "raw")) {
  if (!is_codebook(cb)) stop('`cb` must be an object of class `"li_codebook"`.')
  switch(match.arg(format),
    zapped = attr(cb, "data_zapped"),
    labelled = attr(cb, "data_labelled"),
    raw = attr(cb, "data")
  )
}

#' Write codebook and data summaries to an Excel workbook
#' 
cb_write <- function(cb, 
                     file, 
                     dataset_name,
                     incl_date = TRUE,
                     incl_dims = TRUE,
                     detail_missing = TRUE,
                     group_by = NULL,
                     overwrite = TRUE) {
  summaries <- cb_summaries(cb, detail_missing = detail_missing)
  group_by <- rlang::enquo(group_by)
  if (!rlang::quo_is_null(group_by)) {
    if (detail_missing) {
      cli::cli_inform(c(
        "i" = "Detailed missing value information is not currently supported for grouped summaries."
      ))
    }
    summaries$grouped <- cb_summaries(cb, group_by = !!group_by)
  }
  cb_write_codebook(
    cb, summaries,
    file = file, dataset_name = dataset_name, incl_date = incl_date, 
    incl_dims = incl_dims, detail_missing = detail_missing, 
    group_by = {{ group_by }}, overwrite = overwrite
  )
}



