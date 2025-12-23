
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
#' @param warn_if_none Should a warning be issued if there are no numeric variables
#'   in `cb`?
#'
#' @return If there no numeric variables in `cb`, `NULL`. Otherwise, a tibble with 
#'   columns:
#'   - optional grouping column(s) if specified in `group_by`
#'   - `name`: variable name
#'   - `label_stem`: optional column containing variable label stems; included if
#'      `cb` includes a `label_stem` column and at least one numeric variable has
#'      a non-missing label stem.
#'   - `label`: variable label
#'   - `valid_n`, `valid_pct`: number and proportion of non-missing values
#'   - summary statistic columns: by default, these include `mean` and standard 
#'     deviation (`SD`); `median`, median absolute deviation (`MAD`), `min`, `max`, 
#'     and `range`; skewness (`skew`), and kurtosis (`kurt`).
#'
#' @export
cb_summarize_numeric <- function(cb, group_by = NULL, warn_if_none = TRUE) {
  check_codebook(cb)
  
  data <- attr(cb, "data_zapped")[cb$name]
  nms_num <- names(data)[vapply(data, is.numeric, logical(1))]

  out <- cb |>
    dplyr::filter(name %in% nms_num) |>
    dplyr::select(tidyselect::any_of(c("name", "label_stem", "label")))
  
    if (!nrow(out)) {
    if (warn_if_none) {
      cli::cli_warn(c(
        "!" = "No numeric variables in codebook; returning `NULL`."
      ))
    }
    return(NULL)
  }
  
  # only include `label_stem` if not empty
  if ("label_stem" %in% names(out) && all(is.na(out$label_stem))) {
    out$label_stem <- NULL
  }
    
  res <- lighthouse::summary_table(
      data,
      valid_n = lighthouse::n_valid, valid_pct = lighthouse::pct_valid,
      mean, SD = sd,
      median, MAD = mad, min = lighthouse::min_if_any, max = lighthouse::max_if_any, 
      range = spread_if_any,
      skew = moments::skewness, kurt = moments::kurtosis,
      na.rm = TRUE,
      .vars = tidyselect::all_of(nms_num),
      .rows_group_by = {{ group_by }}
    ) |>
    dplyr::mutate(dplyr::across(
      {{ group_by }},
      \(x) fct_replace_na(factor(x), "(Missing)")
    ))
  
  out <- out |>
    dplyr::left_join(res, dplyr::join_by(name == Variable)) |>
    dplyr::relocate({{ group_by }}) 
  
  group_by_quo <- rlang::enquo(group_by)
  if (!rlang::quo_is_null(group_by_quo)) {
    out <- set_attrs(
      out,
      group_by = group_by_quo, group_counts = group_counts(cb, {{ group_by }})
    )
  }
  
  out
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
#' @param warn_if_none Should a warning be issued if there are no categorical 
#'   variables in `cb`?
#'
#' @return If there no categorical variables in `cb`, `NULL`. Otherwise, a tibble 
#'   with columns:
#'   - optional grouping column(s) if specified in `group_by`
#'   - `name`: variable name
#'   - `label_stem`: optional column containing variable label stems; included if
#'      `cb` includes a `label_stem` column and at least one categorical variable 
#'      has a non-missing label stem.
#'   - `label`: variable label
#'   - `is_missing`: optional column indicating if `value` is a missing value. Included
#'      if `detail_missing` is `TRUE`.
#'   - `value`: variable value
#'   - `n`: number of observations
#'   - `pct_of_all`: proportion of all (non-missing and missing) observations
#'   - `pct_of_valid`: for non-missing values, proportion of all non-missing observations
#'   - `pct_of_missing`: optional column showing, for missing values, proportion 
#'     of all missing observations. Included if `detail_missing` is `TRUE`.
#' 
#' @export
cb_summarize_categorical <- function(cb,
                                     group_by = NULL,
                                     prefixed = TRUE,
                                     detail_missing = missing(group_by),
                                     detail_na_label = "NA",
                                     warn_if_none = TRUE) {
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
  
  if (!length(c(cols_cat, cols_lgl))) {
    if (warn_if_none) {
      if (!length(cols_grp)) {
        cli::cli_warn(c(
          "!" = "No categorical variables in codebook; returning `NULL`."
        ))
      } else {
        cli::cli_warn((
          "i" = "No categorical variables in codebook after grouping; returning `NULL`."
        ))
      }
    }
    return(NULL)
  }
  
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
    if (anyNA(v)) v <- fct_replace_na(v, "(Missing)")
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
    # remove group var combos if no observed values
    _[, .SD[!(all(is.na(n)))], by = cols_grp] |>    
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
  
  out <- freqs |>
    tibble::as_tibble() |>
    set_attrs(detail_missing = detail_missing)
  
  group_by_quo <- rlang::enquo(group_by)
  if (!rlang::quo_is_null(group_by_quo)) {
    out <- set_attrs(
      out,
      group_by = group_by_quo, group_counts = group_counts(cb, {{ group_by }})
    )
  }

  out
}

#' Summarize character variables from a codebook object
#'
#' `cb_summarize_text()` generates a summary table for all character variables 
#' from a codebook object, including number of unique values, frequencies for the 
#' most common values, and missing value information. Note that character variables 
#' of class `"haven_labelled"` are treated as categorical; see `cb_summarize_categorical()`.
#'
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param n_text_vals How many unique non-missing values should be included for 
#'   each variable?
#' @param detail_missing Include detailed missing value information?
#' @param detail_na_label Label used for `NA` values when `detail_missing` is `TRUE`.
#' @param warn_if_none Should a warning be issued if there are no text variables in `cb`?
#' 
#' @return If there no text variables in `cb`, `NULL`. Otherwise, a tibble with 
#'   columns:
#'   - `name`: variable name
#'   - `label_stem`: optional column containing variable label stems; included if
#'      `cb` includes a `label_stem` column and at least one character variable 
#'      has a non-missing label stem.
#'   - `label`: variable label
#'   - `unique_n`: number of unique non-missing values
#'   - `is_missing`: optional column indicating if `value` is a missing value. Included
#'      if `detail_missing` is `TRUE`.
#'   - `value`: the most prevalent unique values for the variable. If there are 
#'     more than `n_text_vals` + 1 unique values, the `n_text_vals` most common
#'     non-missing values will be included. (All missing values will always be included.)
#'   - `n`: number of observations
#'   - `pct_of_all`: proportion of all (non-missing and missing) observations
#'   - `pct_of_valid`: for non-missing values, proportion of all non-missing observations
#'   - `pct_of_missing`: optional column showing, for missing values, proportion 
#'     of all missing observations. Included if `detail_missing` is `TRUE`.
#' 
#' @export
cb_summarize_text <- function(cb,
                              n_text_vals = 5,
                              detail_missing = TRUE,
                              detail_na_label = "NA",
                              warn_if_none = TRUE) {
  check_codebook(cb)

  data <- attr(cb, "data_zapped")[cb$name]
  nms_num <- names(data)[vapply(data, is.numeric, logical(1))]

  data_dt <- data.table::as.data.table(attr(cb, "data_labelled")[cb$name])
  cols_chr <- names(data_dt)[vapply(data_dt, is.character, logical(1))]

  if (!length(cols_chr)) {
    if (warn_if_none) {
      cli::cli_warn("No character variables in codebook; returning `NULL`.")
    }
    return(NULL)
  }

  data_dt <- data_dt[, ..cols_chr]

  cb <- data.table::as.data.table(cb)[name %in% cols_chr]
    
  label_cols <- intersect(c("name", "label_stem", "label"), names(cb))
  var_labs <- cb[, label_cols, with = FALSE]
  ls <- var_labs[["label_stem"]]
  if (!is.null(ls) && all(is.na(ls))) var_labs[, label_stem := NULL]
  
  user_missings <- list()
  if (detail_missing) {
    user_missings <- attr(cb, "user_missing")
    user_missings <- user_missings[intersect(cols_chr, names(user_missings))]
  }
  if (length(user_missings)) {
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
    _[order(match(name, unique(name)), is_missing, -n * !is_missing, value_val)] |>
    _[,
      unique_n := data.table::fifelse(is_missing, NA, .N),
      by = c("name", "is_missing")
    ] |>
    _[,
      value := data.table::fcase(
        !is_missing & .N > n_text_vals + 1 &
          data.table::frankv(n, order = -1, ties.method = "first") > n_text_vals,
        stringr::str_c("(", .N - n_text_vals, " other values)"),
        is.na(value_val) & detail_missing, detail_na_label,
        is.na(value_val), "(Missing)",
        rep(all(is.na(value_lab) | is.na(value_val) | value_lab == value_val), .N), value_val,
        default = stringr::str_c("[", value_val, "] ", data.table::fcoalesce(value_lab, value_val))
      ),
      by = c("name", "is_missing")
    ] |>
    _[,
      list(n = sum(n)),
      by = c("name", "label", "unique_n", "is_missing", "value")] |> 
    _[, pct_of_all := n / sum(n), by = "name"] |>
    _[!(is_missing), pct_of_valid := n / sum(n), by = "name"]
      
  if (detail_missing) {
    freqs[(is_missing), pct_of_missing := n / sum(n), by = "name"]
  } else {
    freqs[, is_missing := NULL]
  }
  
  cols_out <- c(
    "name", "label_stem", "label", "is_missing", "unique_n", "value", "n", 
    "pct_of_all", "pct_of_valid", "pct_of_missing"
  )
  freqs <- freqs[, intersect(cols_out, names(freqs)), with = FALSE]
  
  freqs |>
    tibble::as_tibble() |>
    set_attrs(detail_missing = detail_missing)
}

group_counts <- function(cb, group_by) {
  cb |>
    attr("data_zapped") |>
    dplyr::count(dplyr::pick({{ group_by }})) |>
    dplyr::mutate(dplyr::across(
      {{ group_by }},
      \(x) fct_replace_na(factor(x), "(Missing)")
    )) |>
    deframe_nest()
}
