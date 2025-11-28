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
  dplyr::left_join(out, res, dplyr::join_by(name == Variable))
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
      missing = is.na(value),
      pct_of_all = n / sum(n),
      pct_of_valid = ifelse(missing, NA, n / sum(n[!missing]))
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
    dplyr::arrange(dplyr::pick({{ .by }}), missing)
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
  dplyr::left_join(summary_cat, res, by = "name")
}

cb_gen_summaries <- function(cb, detail_missing = TRUE, group_by = NULL) {
  summaries <- list(
    num = cb_summarize_numeric(cb),
    cat = cb_summarize_categorical(cb, detail_missing = detail_missing)
  )
  group_by <- rlang::enquo(group_by)
  if (!rlang::quo_is_null(group_by)) {
    if (detail_missing) {
      cli::cli_inform(c(
        "i" = "Detailed missing value information is not currently supported for grouped summaries."
      ))
    }
    summaries$grouped <- list(
      num = cb_summarize_numeric(cb, group_by = !!group_by),
      cat = cb_summarize_categorical(
        cb,
        group_by = !!group_by, detail_missing = FALSE
      )
    )
  }
  summaries
}
