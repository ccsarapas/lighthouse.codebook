
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
    dplyr::select(tidyselect::any_of(c("name", "label_stem", "label")))
  # only include `label_stem` if not empty
  if ("label_stem" %in% names(out) && all(is.na(out$label_stem))) {
    out$label_stem <- NULL
  }
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
    }
    labs <- c(labs, NA)
    miss <- c(miss, NA)
    val_labs[[nm]] <- labs
    is_missing[[nm]] <- labs %in% miss
  }
  lgl_labs <- lapply(
    setNames(nm = cols_lgl),
    \(x) setNames(nm = c("TRUE", "FALSE", NA))
  )
  lgl_miss <- lapply(lgl_labs, is.na)
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
  all_vals <- data.table::data.table(
    name = rep(names(val_labs), val_labs_len),
    value_lab = unlist(lapply(unname(val_labs), names)),
    value_val = as.character(unlist(val_labs)),
    is_missing = unlist(is_missing)
  )
  
  label_cols <- intersect(c("label_stem", "label"), names(cb))
  if (length(label_cols)) {
    all_vals <- cb |>
      data.table::as.data.table() |>
      _[, c("name", label_cols), with = FALSE] |>
      merge(all_vals, by = "name", all.y = TRUE, sort = FALSE)
    ls <- all_vals[["label_stem"]]
    if (!is.null(ls) && all(is.na(ls))) all_vals[, label_stem := NULL]
  }
  all_vals <- do.call(expand_dt, c(list(all_vals), grp_labs))

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
    _[, list(n = .N), by = c(cols_grp, "name", "value_val")]
  
  freqs <- merge(
      all_vals, freqs,
      by = c(cols_grp, "name", "value_val"), all = TRUE, sort = FALSE
    ) |> 
    # flag true `NA`s as missing
    _[is.na(value_val), is_missing := TRUE] |>
    # remove missing values if not observed in at least one group
    _[, .SD[!(all(is.na(n)) & is_missing)], by = c("name", "value_val")] |>
    _[order(match(name, unique(name)))] |> 
    _[is.na(n), n := 0L] |>
    _[,
      value := data.table::fcase(
        is.na(value_val) & detail_missing, detail_na_label,
        is.na(value_val), "(Missing)",
        rep(all(is.na(value_lab) | is.na(value_val) | value_lab == value_val), .N), value_val,
        default = stringr::str_c("[", value_val, "] ", data.table::fcoalesce(value_lab, value_val))
      ),
      by = c("name", "is_missing")] |>
    _[, pct_of_all := n / sum(n), by = c(cols_grp, "name")] |>
    _[!(is_missing), pct_of_valid := n / sum(n), by = c(cols_grp, "name")]
      
  if (detail_missing) {
    freqs[(is_missing), pct_of_missing := n / sum(n), by = c(cols_grp, "name")]
  } else {
    freqs[, is_missing := NULL]
  }
  
  cols_out <- c(
    cols_grp, "name", "label_stem", "label", "is_missing", "value", "n", 
    "pct_of_all", "pct_of_valid", "pct_of_missing"
  )
  freqs <- freqs[, intersect(cols_out, names(freqs)), with = FALSE]
  
  freqs |>
    tibble::as_tibble() |>
    set_attrs(
      group_by = rlang::enquo(group_by),
      detail_missing = detail_missing
    )
}

cb_summarize_character <- function(cb,
                                   n_char_vals = 5,
                                   detail_missing = TRUE,
                                   detail_na_label = "NA") {
  check_codebook(cb)
  cb <- data.table::as.data.table(cb)[type == "character"]
  cols_chr <- cb[["name"]]
  data_dt <- attr(cb, "data_labelled") |> 
    data.table::as.data.table() |> 
    _[, cols_chr, with = FALSE]
    
  label_cols <- intersect(c("name", "label_stem", "label"), names(cb))
  var_labs <- cb[, label_cols, with = FALSE]
  ls <- var_labs[["label_stem"]]
  if (!is.null(ls) && all(is.na(ls))) var_labs[, label_stem := NULL]
  
  if (detail_missing) {
    user_missings <- attr(cb, "user_missing")
    user_missings <- user_missings[intersect(cols_chr, names(user_missings))]
    missing_len <- sapply(user_missings, length)    
    all_missings <- data.table::data.table(
        name = rep(names(user_missings), missing_len),
        value_lab = unlist(
          lapply(unname(user_missings), \(um) names(um) %||% rep("", length(um)))
        ),
        value_val = as.character(unlist(user_missings)),
        is_missing = TRUE
      )
    col_to_chr <- as.character
  } else {
    all_missings <- data.table::data.table(
      name = NA_character_, value_lab = NA_character_, 
      value_val = NA_character_, is_missing = NA
    )
    col_to_chr <- \(x) as.character(haven::zap_missing(x))
  }

  for (col in cols_chr) {
    data.table::set(data_dt, j = col, value = col_to_chr(data_dt[[col]]))
  }
  
  freqs <- data.table::melt(
      data_dt,
      measure.vars = names(data_dt),
      variable.name = "name", value.name = "value_val", variable.factor = FALSE
    ) |>
    _[, list(n = .N), by = c("name", "value_val")] |>
    merge(var_labs, by = "name", all.x = TRUE, sort = FALSE) |>
    merge(
      all_missings,
      by = c("name", "value_val"), all.x = TRUE, sort = FALSE
    ) |>
    _[, is_missing := lighthouse::is_TRUE(is_missing) | is.na(value_val)] |>
    # not sure if this is needed here
    _[order(match(name, unique(name)), -n, value_val)] |>
    _[,
      n_vals := data.table::fifelse(is_missing, NA, .N),
      by = c("name", "is_missing")
    ] |>
    _[,
      value := data.table::fcase(
        !is_missing & .N > n_char_vals + 1 &
          data.table::frankv(n, order = -1, ties.method = "first") > n_char_vals,
        stringr::str_c("(", .N - n_char_vals, " other values)"),
        is.na(value_val) & detail_missing, detail_na_label,
        is.na(value_val), "(Missing)",
        rep(all(is.na(value_lab) | is.na(value_val) | value_lab == value_val), .N), value_val,
        default = stringr::str_c("[", value_val, "] ", data.table::fcoalesce(value_lab, value_val))
      ),
      by = c("name", "is_missing")
    ] |>
    _[,
      list(n = sum(n)),
      by = c("name", "label", "n_vals", "is_missing", "value")] |> 
    _[, pct_of_all := n / sum(n), by = "name"] |>
    _[!(is_missing), pct_of_valid := n / sum(n), by = "name"]
      
  if (detail_missing) {
    freqs[(is_missing), pct_of_missing := n / sum(n), by = "name"]
  } else {
    freqs[, is_missing := NULL]
  }
  
  cols_out <- c(
    "name", "label_stem", "label", "n_vals", "is_missing", "value", "n", 
    "pct_of_all", "pct_of_valid", "pct_of_missing"
  )
  freqs <- freqs[, intersect(cols_out, names(freqs)), with = FALSE]
  
  freqs |>
    tibble::as_tibble() |>
    set_attrs(detail_missing = detail_missing)
}
