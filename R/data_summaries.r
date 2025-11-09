
## methods for dates, datetimes, etc?
cb_summarize_numeric <- function(cb, group_cols = NULL) {
  out <- cb |>
    dplyr::filter(type %in% c("numeric", "integer")) |>
    dplyr::select(name, label)
  nms_num <- out$name
  data <- attr(cb, "data_zapped")

  # should be able to just pass `group_cols` directly, but due to bug in
  # `summary_table()`, have to do this for now
  # (see https://github.com/ccsarapas/lighthouse/issues/41)
  if (is.null(group_cols)) {
    res <- summary_table(
      data,
      `Valid n` = n_valid, valid_pct = pct_valid,
      mean, SD = sd,
      median, MAD = mad, min = min_if_any, max = max_if_any, range = spread_if_any,
      skew = moments::skewness, kurt = moments::kurtosis,
      na.rm = TRUE,
      .vars = tidyselect::all_of(nms_num)
    )
  } else {
    res <- summary_table(
      data,
      `Valid n` = n_valid, valid_pct = pct_valid,
      mean, SD = sd,
      median, MAD = mad, min = min_if_any, max = max_if_any, range = spread_if_any,
      skew = moments::skewness, kurt = moments::kurtosis,
      na.rm = TRUE,
      .vars = tidyselect::all_of(nms_num),
      .cols_group_by = {{ group_cols }}
    )
  }
  dplyr::left_join(out, res, dplyr::join_by(name == Variable)) |>
    nan_to_na()
}

cb_count <- function(data,
                     var,
                     prefixed = TRUE,
                     detail_missing = FALSE,
                     detail_na_label = "NA") {
  levels <- if (prefixed) "prefixed" else "labels"
  data <- dplyr::rename(data, value = {{ var }})
  if (!detail_missing) data$value <- labelled::user_na_to_na(data$value)
  na_label <- if (detail_missing) detail_na_label else "(Missing)"
  valid_labs <- val_labels_valid(data$value)
  out <- data |>
    dplyr::count(value) |>
    tidyr::complete(value = valid_labs, fill = list(n = 0)) |>
    dplyr::mutate(name = as.character(rlang::ensym(var)), .before = value) |>
    dplyr::mutate(
      missing = is.na(value),
      value = as.character(labelled::to_factor(
        value,
        levels = levels, user_na_to_na = !detail_missing,
        explicit_tagged_na = detail_missing
      )),
      value = tidyr::replace_na(value, na_label),
      pct_of_all = n / sum(n),
      pct_of_valid = ifelse(missing, NA, n / sum(n[!missing]))
    ) |>
    dplyr::arrange(missing)
  if (detail_missing) {
    out <- out |>
      dplyr::mutate(
        `valid / missing` = ifelse(
          missing,
          sum(n[missing]) / sum(n),
          sum(n[!missing]) / sum(n)
        ),
        `valid / missing` = glue_chr(
          "{ifelse(missing, 'Missing', 'Valid')} ",
          "({sprintf('%.1f%%', `valid / missing` * 100)})"
        ),
        .before = value
      ) |>
      dplyr::mutate(
        pct_of_missing = ifelse(missing, n / sum(n[missing]), NA),
        .after = pct_of_valid
      )
  }
  dplyr::select(out, !missing)
}

cb_count_multiple <- function(data, 
                              ..., 
                              .prefixed = TRUE, 
                              .detail_missing = FALSE, 
                              .detail_na_label = "NA",
                              .no_prefix = NULL) {
  dots <- rlang::list2(...)
  .prefixed <- .prefixed & !(as.character(dots) %in% .no_prefix)
  purrr::map2_dfr(dots, .prefixed, \(v, p) {
    cb_count(
      data, var = {{ v }}, prefixed = p, 
      detail_missing = .detail_missing, detail_na_label = .detail_na_label
    )
  })
}


cb_summarize_categorical <- function(cb, 
                                     prefixed = TRUE, 
                                     detail_missing = TRUE, 
                                     detail_na_label = "NA") {
  summary_cat <- cb |>
    dplyr::filter(type %in% c("factor", "logical")) |>
    dplyr::select(name, label)
  nms_cat <- summary_cat$name
  factors <- attr(cb, "factors")
  data <- attr(cb, "data_labelled")
  cb_count_multiple(
    data, !!!rlang::syms(nms_cat), .prefixed = prefixed, .no_prefix = factors,
    .detail_missing = detail_missing, .detail_na_label = detail_na_label
  )
}


