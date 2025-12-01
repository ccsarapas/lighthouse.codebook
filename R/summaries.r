# think about whether / how to handle missing values in `group_by`

cb_summarize_numeric <- function(cb, group_by = NULL) {
  out <- cb |>
    dplyr::filter(type %in% c("numeric", "integer")) |>
    dplyr::select(name, label)
  nms_num <- out$name
  data <- attr(cb, "data_zapped")
  res <- summary_table(
    data,
    `Valid n` = n_valid, valid_pct = pct_valid,
    mean, SD = sd,
    median, MAD = mad, min = min_if_any, max = max_if_any, range = spread_if_any,
    skew = moments::skewness, kurt = moments::kurtosis,
    na.rm = TRUE,
    .vars = tidyselect::all_of(nms_num),
    .rows_group_by = {{ group_by }}
  )
  out |>
    dplyr::left_join(res, dplyr::join_by(name == Variable)) |>
    set_attrs(group_by = rlang::enquo(group_by))
}

cb_count <- function(data,
                     var,
                     .by = NULL,
                     prefixed = TRUE,
                     detail_missing = FALSE,
                     detail_na_label = "NA") {
  to_fct_chr <- function(x, levels, detail_missing, na_label) {
    x |> 
      labelled::to_factor(
        levels = levels, user_na_to_na = !detail_missing,
        explicit_tagged_na = detail_missing
      ) |>
      as.character() |> 
      tidyr::replace_na(na_label)
  }
  if (detail_missing && !rlang::quo_is_null(rlang::enquo(.by))) {
    cli::cli_abort(c(
      "Detailed missing value information is not currently supported for multiple groups.",
      "i" = "{.code detail_missing} cannot be {.code TRUE} when {.code .by} is specified."      
    ))
  }
  levels <- if (prefixed) "prefixed" else "labels"
  data <- dplyr::rename(data, value = {{ var }})
  if (!detail_missing) data$value <- labelled::user_na_to_na(data$value)
  na_label <- if (detail_missing) detail_na_label else "(Missing)"
  valid_labs <- val_labels_valid(data$value)
  out <- data |>
    dplyr::group_by(dplyr::pick({{ .by }})) |>
    dplyr::count(value) |>
    tidyr::complete(value = valid_labs, fill = list(n = 0)) |>
    dplyr::mutate(
      is_missing = is.na(value),
      pct_of_all = n / sum(n),
      pct_of_valid = ifelse(is_missing, NA, n / sum(n[!is_missing]))
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate(name = as.character(rlang::ensym(var)), .before = value) |>
    dplyr::mutate(
      dplyr::across({{ .by }}, \(x) to_fct_chr(
        x, levels ="labels", detail_missing = detail_missing, 
        na_label = na_label
      )),
      value = to_fct_chr(
        value, levels = levels, detail_missing = detail_missing, 
        na_label = na_label
      )
    ) |>
    dplyr::arrange(dplyr::pick({{ .by }}), is_missing)
  if (detail_missing) {
    out <- out |>
      # dplyr::mutate(
      # # valid_missing = ifelse(is_missing, "Missing", "Valid"),
      #   `valid / missing` = ifelse(
      #     is_missing,
      #     sum(n[is_missing]) / sum(n),
      #     sum(n[!is_missing]) / sum(n)
      #   ),
      #   `valid / missing` = glue_chr(
      #     "{ifelse(is_missing, 'Missing', 'Valid')} ",
      #     "({sprintf('%.1f%%', `valid / missing` * 100)})"
      #   ),
      #   .before = value
      # ) |>
      dplyr::mutate(
        pct_of_missing = ifelse(is_missing, n / sum(n[is_missing]), NA),
        .after = pct_of_valid
      )
    # }
  } else {
    out$is_missing <- NULL
  }
  out
  # dplyr::select(out, !is_missing)
}

cb_count_multiple <- function(data, 
                              ..., 
                              .by = NULL,
                              .prefixed = TRUE, 
                              .detail_missing = FALSE,
                              .detail_na_label = "NA",
                              .no_prefix = NULL) {
  dots <- rlang::list2(...)
  .prefixed <- .prefixed & !(as.character(dots) %in% .no_prefix)
  purrr::map2_dfr(dots, .prefixed, \(v, p) {
    cb_count(
      data, var = {{ v }}, prefixed = p, detail_missing = .detail_missing, 
      detail_na_label = .detail_na_label, .by = {{ .by }}
    )
  })
}


cb_summarize_categorical <- function(cb, 
                                     group_by = NULL,
                                     prefixed = TRUE, 
                                     detail_missing = missing(group_by), 
                                     detail_na_label = "NA") {
  factors <- attr(cb, "factors")
  data <- attr(cb, "data_labelled")
  group_chr <- untidyselect(data, {{ group_by }})
  summary_cat <- cb |>
    dplyr::filter(
      type %in% c("factor", "logical"),
      !(name %in% group_chr)
    ) |>
    dplyr::select(name, label)
  vars <- summary_cat$name
  res <- cb_count_multiple(
    data, !!!vars, .prefixed = prefixed, .no_prefix = factors,
    .detail_missing = detail_missing, .detail_na_label = detail_na_label, 
    .by = {{ group_by }}
  )
  summary_cat |>
    dplyr::left_join(res, by = "name") |>
    set_attrs(group_by = rlang::enquo(group_by), detail_missing = detail_missing)
}
