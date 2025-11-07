
## methods for dates, datetimes, etc?
cb_summarize_numeric <- function(cb) {
  out <- cb |>
    dplyr::filter(type == "numeric") |>
    dplyr::select(name, label)
  nms_num <- out$name
  data <- attr(cb, "data_zapped")

  res <- summary_table(
    data,
    `valid n` = n_valid, `valid %` = pct_valid,
    mean, SD = sd,
    median, MAD = mad, min = min_if_any, max = max_if_any, range = spread_if_any,
    skew = moments::skewness, kurt = moments::kurtosis,
    na.rm = TRUE,
    .vars = tidyselect::all_of(nms_num)
  )
  dplyr::left_join(out, res, dplyr::join_by(name == Variable)) |>
    nan_to_na()
}

cb_count <- function(data,
                     var,
                     prefixed = TRUE,
                     detail_missing = FALSE,
                     detail_na_label = "NA") {
  levels <- if (prefixed) "prefixed" else "labels"
  data <- dplyr::rename(data, Value = {{ var }})
  if (!detail_missing) data$Value <- labelled::user_na_to_na(data$Value)
  na_label <- if (detail_missing) detail_na_label else "(Missing)"
  valid_labs <- val_labels_valid(data$Value)
  out <- data |>
    dplyr::count(Value) |>
    tidyr::complete(Value = valid_labs, fill = list(n = 0)) |>
    dplyr::mutate(Variable = as.character(rlang::ensym(var)), .before = Value) |>
    dplyr::mutate(
      missing = is.na(Value),
      Value = as.character(labelled::to_factor(
        Value,
        levels = levels, user_na_to_na = !detail_missing,
        explicit_tagged_na = detail_missing
      )),
      Value = tidyr::replace_na(Value, na_label),
      pct_of_all = n / sum(n),
      pct_of_valid = ifelse(missing, NA, n / sum(n[!missing]))
    ) |>
    dplyr::arrange(missing)
  if (detail_missing) {
    out <- out |>
      dplyr::mutate(
        pct_valid_miss = ifelse(
          missing,
          sum(n[missing]) / sum(n),
          sum(n[!missing]) / sum(n)
        ),
        pct_valid_miss = glue_chr(
          "{ifelse(missing, 'Missing', 'Valid')} ",
          "({sprintf('%.1f%%', pct_valid_miss * 100)})"
        ),
        .after = Value
      ) |>
      dplyr::mutate(
        pct_of_missing = ifelse(missing, n / sum(n[missing]), NA),
        .after = pct_of_valid
      )
  }
  out
}

cb_count_multiple <- function(data, 
                              ..., 
                              .prefixed = TRUE, 
                              .detail_missing = FALSE, 
                              .detail_na_label = "NA") {
  dots <- rlang::list2(...)
  purrr::map_dfr(
    dots, cb_count, data = data, prefixed = .prefixed,
    detail_missing = .detail_missing, detail_na_label = .detail_na_label
  )
}


cb_summarize_categorical <- function(cb, 
                                     prefixed = TRUE, 
                                     detail_missing = TRUE, 
                                     detail_na_label = "NA") {
  summary_cat <- cb |>
    dplyr::filter(type %in% c("factor", "logical")) |>
    dplyr::select(name, label)
  nms_cat <- summary_cat$name
  data <- attr(cb, "data_labelled")
  cb_count_multiple(
    data, !!!rlang::syms(nms_cat), .prefixed = prefixed, 
    .detail_missing = detail_missing, .detail_na_label = detail_na_label
  )
}

