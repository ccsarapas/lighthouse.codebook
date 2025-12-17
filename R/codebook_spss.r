
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
