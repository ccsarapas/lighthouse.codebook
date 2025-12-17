cb_init <- function(data, 
                    meta = NULL, 
                    meta_var_name = NULL, 
                    meta_var_label = NULL, 
                    meta_val_labels = NULL,
                    ...) {
  out <- tibble::tibble(name = names(data))
  if (!is.null(meta)) {
    meta_var_name <- rlang::enquo(meta_var_name)
    if (rlang::quo_is_null(meta_var_name)) {
      cli::cli_abort(
        "{.code meta_var_name} cannot be `NULL`if {.code meta} is provided."
      )
    }
    out <- out |>
      dplyr::left_join(meta, dplyr::join_by(name == !!meta_var_name)) |>
      dplyr::select(
        name,
        label = {{ meta_var_label }}, value_labels = {{ meta_val_labels }},
        ...
      )
  } else {
    out <- out |>
      dplyr::mutate(value_labels = NA_character_)
  }
  out <- set_attrs(out, data = data)
  class(out) <- c("li_codebook", class(out))
  out
}

cb_clean_fields <- function(cb, rmv_html = TRUE, rmv_line_breaks = TRUE) {
  if (rmv_html) {
    cb <- dplyr::mutate(cb, dplyr::across(!name, strip_html))
  }
  if (rmv_line_breaks) {
    cb <- dplyr::mutate(cb, dplyr::across(!name, strip_line_breaks))
  }
  cb
}

cb_user_missings_by_var <- function(cb, 
                                    user_missing = list(), 
                                    match_type = TRUE,
                                    incompatible = c("ignore", "warn", "error")) {
  incompatible <- match.arg(incompatible)
  user_missing_names <- names(user_missing)
  if (!rlang::is_bare_list(user_missing) || is.null(user_missing_names)) {
    cli::cli_abort(c(
      "!" = "{.arg .user_missing} must be a named list",
      "i" = "To set the same missing vals across multiple variables, try {.code cb_user_missings_across()}."
    ))
  }
  data <- attr(cb, "data")
  idx_labelable <- can_have_labels(user_missing_names, data)
  if (incompatible != "ignore" && !all(idx_labelable)) {
    n_bad <- sum(!idx_labelable)
    bad_vars <- user_missing_names[!idx_labelable]
    bad_vars <- vapply(
      bad_vars,
      \(v) cli::format_inline("{.var {v}} {.cls {class(data[[v]])}}"),
      character(1)
    )
    if (length(bad_vars) > 4) bad_vars <- c(head(bad_vars, 3), "...")
    bad_vars <- paste(bad_vars, collapse = ", ")
    msg <- "{n_bad} variable{?s} specified in {.arg .user_missing} are not compatible with user missing values"
    if (incompatible == "error") cli::cli_abort(c("!" = msg, "*" = bad_vars))
    cli::cli_warn(c("!" = paste0(msg, " and will be ignored"), "*" = bad_vars))
  }
  user_missing <- user_missing[idx_labelable]
  user_missing_names <- user_missing_names[idx_labelable]
  if (!all(user_missing_names %in% cb$name)) {
    cli::cli_abort("{.code user_missing} contains names not found in {.code cb}.")
  }
  user_missing0 <- attr(cb, "user_missing")
  n_shared <- sum(user_missing_names %in% names(user_missing0))
  if (n_shared) {
    cli::cli_warn(c(
      "!" = "Existing user missing values will be overwritten for {n_shared} variable{?s}."
    ))
  }
  if (match_type) {
    user_missing <- lapply(
      setNames(nm = names(user_missing)), 
      \(nm, data) as_named(user_missing[[nm]], cb_match_type(nm, data)),
      data = data
    )
  }
  keep_prev <- user_missing0[!(names(user_missing0) %in% user_missing_names)]
  user_missing <- c(user_missing, keep_prev)
  set_attrs(cb, user_missing = user_missing)
}

cb_user_missings_across <- function(cb,
                                    user_missing,
                                    vars = tidyselect::everything(),
                                    match_type = TRUE,
                                    incompatible = c("ignore", "warn", "error")) {
  data <- attr(cb, "data")
  vars <- rlang::enexpr(vars) %||% rlang::expr(tidyselect::everything())
  vars <- setNames(nm = untidyselect(data, !!vars))
  user_missing_list <- lapply(vars, \(x) user_missing)
  cb_user_missings_by_var(
    cb, user_missing = user_missing_list, match_type = match_type, 
    incompatible = incompatible
  )
}

cb_user_missings <- function(cb, 
                             user_missing, 
                             match_type = TRUE,
                             incompatible = c("ignore", "warn", "error")) {
  if (is.null(user_missing)) return(set_attrs(cb, user_missing = list()))
  user_missing <- check_user_missing_arg(user_missing)
  for (um in user_missing) {
    cb <- cb_user_missings_across(
      cb, user_missing = eval(rlang::f_rhs(um)), vars = !!rlang::f_lhs(um), 
      match_type = match_type, incompatible = incompatible
    )
  }
  cb
}

cb_add_factors <- function(cb, data, except = character()) {
  data <- data[setdiff(names(data), except)]
  data_fct <- data[vapply(data, is.factor, logical(1))]
  factors <- names(data_fct)
  ordered <- factors[vapply(data_fct, is.ordered, logical(1))]
  set_attrs(cb, factors = factors, ordered = ordered)
}

lookups_from_string <- function(cb, data, sep1, sep2) {
  fx_inner <- function(x, nm, type, sep1) {
    if (length(x) == 1 && is.na(x)) {
      return(NA)
    }
    if (!all(stringr::str_detect(x, sep1))) {
      cli::cli_abort("Failed to parse value labels for {.code {nm}}")
    }
    x <- stringr::str_split(x, sep1, simplify = TRUE)
    vals <- as(x[, 1], type)
    labs <- x[, 2]
    setNames(vals, labs)
  }
  val_labels <- na.omit(setNames(cb$value_labels, cb$name))
  if (!length(val_labels)) return(val_labels)
  if (is.null(sep1) || is.null(sep2)) {
    cli::cli_abort(
      "{.arg sep1} and {.arg sep2} must be specified if value labels are provided."
    )
  }
  nms <- names(val_labels)
  types <- vapply(nms, cb_match_type, character(1), data = data)
  num_val <- "-?\\d{1,99}"
  # sep2 followed immediately by number and sep1 (with optional whitespace)
  sep2 <- glue_chr("\\s*{sep2}\\s*(?={num_val}{sep1})")
  # after splitting by sep2, substring begins with number which immediately 
  # precedes sep1 (with optional whitespace)
  sep1 <- glue_chr("(?<=^{num_val})\\s*{sep1}\\s*")
  val_labels <- setNames(stringr::str_split(val_labels, sep2), nms)
  mapply(
    fx_inner, 
    val_labels, nms, types, MoreArgs = list(sep1 = sep1), 
    SIMPLIFY = FALSE
  )
}

lookups_from_labelled <- function(cb, data, except = character()) {
  val_labels <- labelled::val_labels(data)
  val_labels <- val_labels[!vapply(val_labels, is.null, logical(1))]
  val_labels[setdiff(names(val_labels), except)]
}

lookups_from_factor <- function(cb, data) {
  factors <- attr(cb, "factors")
  val_labels_fct <- lapply(data[factors], \(x) setNames(nm = levels(x)))
}

cb_add_lookups <- function(cb, sep1, sep2) {
  data <- attr(cb, "data")
  val_labs_str <- cb |>
    lookups_from_string(data = data, sep1 = sep1, sep2 = sep2)
  val_labs_haven <- cb |>
    lookups_from_labelled(data = data, except = names(val_labs_str))
  cb <- cb |>
    cb_add_factors(data, except = c(names(val_labs_str), names(val_labs_haven)))
  val_labs_fct <- cb |>
    lookups_from_factor(data)
  
  by_label <- c(val_labs_str, val_labs_haven, val_labs_fct)
  by_label <- by_label[order(match(names(by_label), cb$name))]
  by_value <- lapply(by_label, \(x) setNames(names(x), x))
  
  set_attrs(cb, vals_by_label = by_label, labs_by_value = by_value)
}

reconcile_missing_labels <- function(val_labs, 
                                     missings, 
                                     conflict = c("metadata", "missing_label")) {
  conflict <- match.arg(conflict)
  
  labs_in_missing <- val_labs[match(missings, val_labs)]
  
  miss_name <- names_if_any(missings)
  lab_name <- names_if_any(labs_in_missing)
  lab_val <- unname(labs_in_missing)

  add_to_vals <- !is.na(miss_name) & is.na(lab_val)
  label_miss <- !is.na(lab_name) & is.na(miss_name)
  mismatch <- sapply(miss_name != lab_name, isTRUE)

  ### if na is labelled and not in vals
  # add to vals w label
  val_labs <- c(val_labs, missings[add_to_vals])
  ### if val is labelled and na isn't
  # add label to na
  names(missings)[label_miss] <- lab_name[label_miss]
  ### if na is labelled and in vals and labels don't match
  # relabel based on `conflict`
  if (conflict == "metadata") {
    names(missings)[mismatch] <- lab_name[mismatch]
  } else if (conflict == "missing_label") {
    names(val_labs)[match(lab_val[mismatch], val_labs)] <- miss_name[mismatch]
  }
  list(val_labs = val_labs, missings = missings)
}

cb_label_data <- function(cb, conflict = c("metadata", "missing_label")) {
  conflict <- match.arg(conflict)
  data <- attr(cb, "data")
  vals_by_label <- attr(cb, "vals_by_label")
  factors <- attr(cb, "factors")
  user_missing <- attr(cb, "user_missing")
  label_vars <- unique(c(names(vals_by_label), names(user_missing)))
  for (nm in label_vars) {
    missings <- sort(user_missing[[nm]])
    if (nm %in% factors) {
      data[[nm]] <- to_labelled_chr(data[[nm]], na_values = missings)
    } else {
      val_labs <- sort(vals_by_label[[nm]])
      if (!is.null(val_labs) && !is.null(missings)) {
        vals <- reconcile_missing_labels(
          val_labs = val_labs,
          missings = missings,
          conflict = conflict
        )
        missings <- sort(vals$missings)
        val_labs <- vals$val_labs[
          order(vals$val_labs %in% vals$missings, vals$val_labs)
        ]
      }
      data[[nm]] <- haven::labelled_spss(
        data[[nm]], labels = val_labs, na_values = missings
      )
    }
  }
  set_attrs(cb, data_labelled = data)
}

cb_zap_data <- function(cb) {
  data <- attr(cb, "data_labelled")
  ordered <- attr(cb, "ordered")
  data <- data |>
    dplyr::mutate(dplyr::across(tidyselect::where(has_val_labels), \(x) {
      labelled::to_factor(
        x, user_na_to_na = TRUE, ordered = dplyr::cur_column() %in% ordered, 
        sort_levels = "values"
      )
    })) |>
    haven::zap_missing() |>
    haven::zap_labels()
  set_attrs(cb, data_zapped = data)
}

cb_add_dims <- function(cb) {
  data <- attr(cb, "data")
  set_attrs(cb, n_obs = nrow(data), n_vars = ncol(data))
}

string_from_lookups <- function(lookups, no_prefix = NULL) {
  vapply(names(lookups), \(var) {
    x <- lookups[[var]]
    if (is.null(x)) return(NA_character_)
    if (var %in% no_prefix) return(stringr::str_c(x, collapse = "; "))
    labs <- names(x) %||% ""
    labs[is.na(labs)] <- ""
    labs <- ifelse(labs != "", stringr::str_c(" ", labs), labs)
    # labs <- names(x)
    # labs <- if (is.null(labs)) "" else stringr::str_c(" ", tidyr::replace_na(labs, ""))
    stringr::str_c(glue_chr("[{x}]{labs}"), collapse = "; ")
  }, character(1))
}


cb_add_val_labels_col <- function(cb, user_missing_col = c("if_any", "yes", "no")) {
  user_missing_col <- match.arg(user_missing_col)
  data <- attr(cb, "data_labelled")[cb$name]
  val_labs <- labelled::val_labels(data)
  missings <- labelled::na_values(data)
  user_missing_col <- user_missing_col == "yes" || (
    user_missing_col == "if_any" & !(
      rlang::is_empty(missings) || all(vapply(missings, rlang::is_empty, logical(1)))
    )
  )
  if (user_missing_col) {
    missings <- lapply(missings, try_sort_numeric)
    val_labs <- mapply(\(v, m) v[!(v %in% m)], v = val_labs, m = missings)
    missings <- string_from_lookups(missings)
  } else {
    missings <- NULL
  }
  val_labs <- string_from_lookups(val_labs, no_prefix = attr(cb, "factors"))
  dplyr::mutate(cb, value_labels = val_labs, user_missings = missings)
}

cb_split_labels_col <- function(cb, split_var_labels = NULL) {
  if (is.null(split_var_labels)) return(cb)
  if (rlang::is_call(split_var_labels) && rlang::call_name(split_var_labels) == "list") {
    split_var_labels <- rlang::call_args(split_var_labels)
  } else {
    split_var_labels <- list(split_var_labels)
  }
  data <- attr(cb, "data")
  split_var_labels <- lapply(split_var_labels, \(x) untidyselect(data, !!x))
  if (anyDuplicated(unlist(split_var_labels))) {
    cli::cli_abort(
      "The same variable(s) are captured by more than one expression in `split_var_labels`."
    )
  }
  cb <- dplyr::mutate(cb, label_stem = NA_character_, .before = label)
  for (v in split_var_labels) {
    idx <- cb$name %in% v
    stem <- lighthouse::str_prefix(cb$label[idx])
    if (is.na(stem) || stem == "") {
      cli::cli_warn(c(
        "!" = "No common prefix found for variables:",
        "*" = "{toString(v)}"
      ))
    } else {
      cb$label_stem[idx] <- stem
      cb$label[idx] <- stringr::str_remove(cb$label[idx], stringr::fixed(stem))
    }
  }
  cb
}

## should maybe generalize this pattern of [get attr data] -> [sort by cb$name] -> [sapply fx]
cb_add_type_col <- function(cb) {
  data <- attr(cb, "data_zapped")[cb$name]
  cb |>
    dplyr::mutate(
      type = vapply(data, class_collapse, character(1)), 
      type = stringr::str_replace(type, "ordered, factor", "ordered"),
      .after = name
    )
}

cb_add_missing_col <- function(cb) {
  data <- attr(cb, "data_zapped")[cb$name]
  dplyr::mutate(cb, missing = vapply(data, \(x) mean(is.na(x)), double(1)))
}

