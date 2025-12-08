
#' Summarize numeric variables from a codebook object
#' 
#' `cb_summarize_numeric()` generates a summary table for all numeric variables
#' from a codebook object, optionally by group. Future releases will include options
#' to specify the summary statistics used. Currently, summary statistics are valid
#' n and %; mean and SD; median, MAD, min, max, and range; skewness, and kurtosis.
#'
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param group_by <[`tidy-select`][dplyr_tidy_select]> Column or columns to group
#'   by.
#' 
#' @return A tibble with summary statistics for each numeric variable.
#' 
#' @export
cb_summarize_numeric <- function(cb, group_by = NULL) {
  check_codebook(cb)
  out <- cb |>
    dplyr::filter(type %in% c("numeric", "integer")) |>
    dplyr::select(tidyselect::any_of(c("name", "label")))
  nms_num <- out$name
  data <- attr(cb, "data_zapped")
  res <- lighthouse::summary_table(
    data,
    `Valid n` = lighthouse::n_valid, valid_pct = lighthouse::pct_valid,
    mean, SD = sd,
    median, MAD = mad, min = lighthouse::min_if_any, max = lighthouse::max_if_any, range = spread_if_any,
    skew = moments::skewness, kurt = moments::kurtosis,
    na.rm = TRUE,
    .vars = tidyselect::all_of(nms_num),
    .rows_group_by = {{ group_by }}
  )
  out |>
    dplyr::left_join(res, dplyr::join_by(name == Variable)) |>
    set_attrs(group_by = rlang::enquo(group_by))
}

#' Summarize categorical variables from a codebook object
#'
#' `cb_summarize_categorical()` generates a frequencies table for all categorical
#' variables from a codebook object, optionally by group. Variables with value labels,
#' factors (including ordered factors), and logical variables are treated as categorical.
#'
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param group_by <[`tidy-select`][dplyr_tidy_select]> Column or columns to group
#'   by.
#' @param prefixed Should value labels be prefixed with the corresponding value?
#'   e.g., `TRUE` yields `"[1] Value One"`; `FALSE` yields `"Value One"`.
#' @param detail_missing Include detailed missing value information? Currently supported
#'   only when no grouping variables are specified.
#' @param detail_na_label Label used for `NA` values when `detail_missing` is `TRUE`.
#'
#' @return A tibble with frequency information for each categorical variable.
#'
#' @export
cb_summarize_categorical <- function(cb,
                                     group_by = NULL,
                                     prefixed = TRUE,
                                     detail_missing = missing(group_by),
                                     detail_na_label = "NA") {
  check_codebook(cb)
  factors <- attr(cb, "factors")
  val_labs <- attr(cb, "vals_by_label")
  data <- attr(cb, "data_labelled")
  data_dt <- data.table::as.data.table(data)
  
  ## define column groups
  val_labs <- attr(cb, "vals_by_label")
  cols_grp <- untidyselect(data, {{ group_by }})
  cols_cat <- setdiff(names(val_labs), cols_grp)
  cols_lgl <- setdiff(
    names(data_dt)[vapply(data_dt, is.logical, logical(1))],
    cols_grp
  )

  ## define labels and is_missing
  val_labs <- val_labs[cols_cat]
  is_missing <- list()
  user_missings <- attr(cb, "user_missing")
  for (nm in names(val_labs)) {
    labs <- val_labs[[nm]]
    miss <- user_missings[[nm]]
    if (!is.null(miss)) {
      if (detail_missing) {
        if (nm %in% factors) {
          labs <- c(labs[!(labs %in% miss)], miss)
        } else {
          labs <- c(labs, miss[!(miss %in% labs)])
        }
      } else {
        labs <- labs[!(labs %in% miss)]
      }
      val_labs[[nm]] <- labs
    }
    is_missing[[nm]] <- labs %in% miss
  }
  lgl_labs <- lapply(
    setNames(nm = cols_lgl), 
    \(x) setNames(nm = c("TRUE", "FALSE"))
  )
  lgl_miss <- lapply(setNames(nm = cols_lgl), \(x) c(FALSE, FALSE))
  val_labs <- c(val_labs, lgl_labs)
  is_missing <- c(is_missing, lgl_miss)
  val_labs_len <- sapply(val_labs, length)

  data_dt <- data_dt[,
    c(cols_grp, cols_cat, cols_lgl),
    with = FALSE
  ]
  
  ## define group labels  
  grp_labs <- list()
  for (col in cols_grp) {
    v <- data_dt[[col]]
    v <- labelled::to_factor(v, sort_levels = "values", user_na_to_na = TRUE)
    if (anyNA(v)) v <- forcats::fct_na_value_to_level(v, "(Missing)")
    grp_labs[[col]] <- sort(unique(v))
    data.table::set(data_dt, j = col, value = v)
  }
  var_labs <- data.table::as.data.table(cb)[, .(name, label)]
  all_vals <- data.table::data.table(
    name = rep(names(val_labs), val_labs_len),
    value_lab = unlist(lapply(unname(val_labs), names)),
    value_val = as.character(unlist(val_labs)),
    is_missing = unlist(is_missing)
  )
  all_vars <- merge(var_labs, all_vals, by = "name", all.y = TRUE, sort = FALSE)
  all_vars <- do.call(expand_dt, c(list(all_vars), grp_labs))

  if (detail_missing) {
    col_to_chr <- as.character
  } else {
    col_to_chr <- \(x) as.character(haven::zap_missing(x))
  }

  for (col in c(cols_cat, cols_lgl)) {
    data.table::set(data_dt, j = col, value = col_to_chr(data_dt[[col]]))
  }
  
  freqs <- data.table::melt(
      data_dt, id.vars = cols_grp, variable.name = "name", 
      value.name = "value_val", variable.factor = FALSE
    ) |> 
    _[, .(n = .N), by = c(cols_grp, "name", "value_val")]
  
  freqs <- merge(
      all_vars, freqs,
      by = c(cols_grp, "name", "value_val"), all = TRUE, sort = FALSE
    ) |> 
    _[is.na(value_val), is_missing := TRUE] |>
    _[!(is.na(n) & is_missing), ] |>
    _[order(match(name, unique(name)))] |> 
    _[is.na(n), n := 0L] |>
    _[,
      value := data.table::fcase(
        is.na(value_val) & detail_missing, detail_na_label,
        is.na(value_val), "(Missing)",
        rep(all(is.na(value_lab) | is.na(value_val) | value_lab == value_val), .N), value_val,
        default = stringr::str_c("[", value_val, "] ", data.table::fcoalesce(value_lab, value_val))
      ),
      by = c("name", "is_missing")
    ] |>
    _[, c(cols_grp, "name", "label", "is_missing", "value", "n"), with = FALSE] |>
    _[, pct_of_all := n / sum(n), by = c(cols_grp, "name")] |>
    _[!(is_missing), pct_of_valid := n / sum(n), by = c(cols_grp, "name")]

  if (detail_missing) {
    freqs[(is_missing), pct_of_missing := n / sum(n), by = c(cols_grp, "name")]
  } else {
    freqs[, is_missing := NULL]
  }
  freqs |>
    tibble::as_tibble() |>
    set_attrs(
      group_by = rlang::enquo(group_by),
      detail_missing = detail_missing
    )
}
