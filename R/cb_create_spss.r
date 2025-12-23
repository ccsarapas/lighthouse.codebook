#' Generate a codebook object from an SPSS dataset
#'
#' @description
#' `cb_create_spss()` builds an object of class `"li_codebook"` from an SPSS dataset
#' (imported using `haven::read_spss()`, `read_sav()`, or `read_por()`). Metadata
#' including variable labels, value labels, and user missing values are extracted
#' from the imported dataset. (User missing values can also be set using the `.user_missing`
#' argument.)`
#'
#' The resulting object can be used to write an Excel workbook with variable and
#' data summaries (using [`cb_write()`]), extract processed data ([`cb_get_data()`]),
#' or generate dataset summaries ([`cb_summarize_numeric()`], [`cb_summarize_categorical()`],
#' [`cb_summarize_text()`]).
#'
#' @inheritParams cb_create
#' @param data A data frame exported or retrieved from REDCap.
#' @param .user_missing A formula or list of formulas specifying user missing values.
#'   Formulas should specify variables on the left-hand side (as variable names
#'   or [tidyselect][dplyr_tidy_select] expressions), and missing values on the
#'   right-hand side. If left-hand side is omitted, defaults to `tidyselect::everything()`.
#'   See "Specifying user missing values" in [`cb_create()`] documentation for examples.
#' @param .user_missing_conflict If labels passed to `.user_missing` conflicts with
#'   a value label in the `data`, which should be used?
#' 
#' @return
#' An `"li_codebook"` object, consisting of (1) a tibble summarizing the passed
#' dataset and (2) attributes containing the passed dataset (in several formats)
#' and additional metadata. Specifically:
#' - A tibble with columns:
#'     - `name`: variable name
#'     - `type`: optional column containing simplified variable type
#'     - `class`: optional column containing class(es) of each variable
#'     - `label_stem`: optional column containing variable label stems, if any variables 
#'       are specified in `.split_var_labels`
#'     - `label`: variable label
#'     - `value_labels`: value labels
#'     - `user_missing`: optional column, depending on value of `.user_missing_col`,
#'        with value labels for user missing values
#'     - `missing`: proportion missing
#' - Attributes:
#'     - Transformed versions of the passed dataset. See [`cb_get_data()`].
#'     - Lookup tables and other metadata used internally.
#'
#' @export
cb_create_spss <- function(data,
                           .user_missing = NULL,
                           .split_var_labels = NULL,
                           .include_types = !.include_r_classes,
                           .include_r_classes = FALSE,
                           # these would require a different implementation -- omitted for now
                           #   .rmv_html = TRUE,
                           #   .rmv_line_breaks = TRUE,
                           .user_missing_col = c("if_any", "yes", "no"),
                           .user_missing_conflict = c("val_label", "missing_label"),
                           .user_missing_incompatible = c("ignore", "warn", "error")
                           ) {
  data |>
    cb_init() |>
    cb_add_label_col_spss() |>
    cb_update_labels_spss(
      user_missing = .user_missing,
      user_missing_conflict = .user_missing_conflict,
      user_missing_incompatible = .user_missing_incompatible
    ) |>
    cb_zap_data_spss() |>
    cb_add_dims() |>
    cb_add_val_labels_col(user_missing_col = .user_missing_col) |>
    cb_add_type_col(
      include_r_classes = .include_r_classes,
      include_types = .include_types
    ) |>
    cb_add_missing_col() |>
    cb_split_labels_col(split_var_labels = rlang::enexpr(.split_var_labels))
}

cb_user_missings_from_spss <- function(cb) {
  user_missings <- labelled::na_values(attr(cb, "data"))
  user_missings <- user_missings[!sapply(user_missings, is.null)]
  if (length(user_missings)) attr(cb, "user_missing") <- user_missings
  cb
}

cb_update_labels_spss <- function(cb,
                                  user_missing = NULL,
                                  user_missing_conflict = c("val_label", "missing_label"),
                                  user_missing_incompatible = c("ignore", "warn", "error")) {
  data <- attr(cb, "data")
  if (is.null(user_missing)) {
    cb |>
      cb_add_lookups() |>
      set_attrs(data_labelled = data)
  } else {
    conflict <- sub("val_label", "metadata", match.arg(user_missing_conflict))
    user_missing <- check_user_missing_arg(user_missing)
    user_missing_vars <- user_missing |>
      lapply(\(um) {
        vars <- rlang::f_lhs(um) %||% rlang::expr(tidyselect::everything())
        untidyselect(data, !!vars)
      }) |>
      unlist() |>
      unique()
    cb <- cb |>
      cb_user_missings_from_spss() |>
      cb_user_missings(
        user_missing = user_missing,
        incompatible = user_missing_incompatible
      ) |>
      cb_add_lookups()

    # temporarily filter missing and val attributes, so `cb_label_data` only
    # relabels vars for which new user missings were passed
    attr_user_missing <- attr(cb, "user_missing")
    attr_vals_by_label <- attr(cb, "vals_by_label")
    cb |>
      set_attrs(
        user_missing = attr_user_missing[names(attr_user_missing) %in% user_missing_vars],
        vals_by_label = attr_vals_by_label[names(attr_vals_by_label) %in% user_missing_vars]
      ) |>
      cb_label_data(conflict = conflict) |>
      # then restore full missing and val attributes
      set_attrs(
        user_missing = attr_user_missing,
        vals_by_label = attr_vals_by_label
      )
  }
}

cb_zap_data_spss <- function(cb) {
  data <- attr(cb, "data_labelled")
  ordered <- attr(cb, "ordered")
  data <- data |>
    haven::zap_missing() |>
    haven::zap_label() |>
    haven::zap_formats() |>
    haven::zap_widths()
  data_values <- data |>
    haven::zap_labels()
  data_zapped <- data |>
    dplyr::mutate(dplyr::across(tidyselect::where(has_val_labels), \(x) {
      labelled::to_factor(
        x,
        ordered = dplyr::cur_column() %in% ordered, sort_levels = "values"
      )
    })) |>
    haven::zap_labels()
  set_attrs(cb, data_values = data_values, data_zapped = data_zapped)
}
cb_add_label_col_spss <- function(cb) {
  var_labs <- cb |> 
    attr("data") |> 
    labelled::var_label() |>
    lighthouse::null_to_na(unlist = TRUE)
  dplyr::mutate(cb, label = var_labs[name], .before = value_labels)
}
