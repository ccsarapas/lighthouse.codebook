cb_init <- function(data, 
                    meta, 
                    meta_var_name, 
                    meta_var_label = NULL, 
                    meta_val_labels = NULL,
                    ...) {
  tibble::tibble(name = datanames) |>
    dplyr::left_join(meta, dplyr::join_by(name == {{ meta_var_name }})) |>
    dplyr::select(
      name, label = {{ meta_var_label }}, value_labels = {{ meta_val_labels }},
      ...
    ) |> 
    set_attrs(data = data)
}

cb_clean_fields <- function(cb, rmv_html = !name, rmv_line_breaks = !name) {
  cb |>
    dplyr::mutate(
      dplyr::across({{ rmv_html }}, strip_html),
      dplyr::across({{ rmv_line_breaks }}, strip_line_breaks)
    )
}

lookups_from_string <- function(val_labels, types, sep1, sep2) {
  fx_inner <- function(x, nm, type, sep1) {
    if (length(x) == 1 && is.na(x)) return(NA)
    if (!all(stringr::str_detect(x, sep1))) {
      cli::cli_abort("Failed to parse value labels for {.code {nm}}")
    }
    x <- stringr::str_split(x, sep1, simplify = TRUE)
    vals <- as(x[, 1], type)
    labs <- x[, 2]
    setNames(vals, labs)
  }
  num_val <- "-?\\d{1,99}"
  sep2 <- glue_chr("{sep2}(?={num_val}{sep1})")
  sep1 <- glue_chr("(?<=^{num_val}){sep1}")
  nms <- names(val_labels)
  val_labels <- setNames(stringr::str_split(val_labels, sep2), nms)
  mapply(
    fx_inner, 
    val_labels, nms, types, MoreArgs = list(sep1 = sep1), 
    SIMPLIFY = FALSE
  )
}

cb_user_missings_by_var <- function(cb, 
                                     user_missing = list(), 
                                     match_type = TRUE) {
  user_missing_names <- names(user_missing)
  if (!rlang::is_bare_list(user_missing) || is.null(user_missing_names)) {
    cli::cli_abort(c(
      "!" = "{.code user_missing} must be a named list",
      "i" = "To set the same missing vals across multiple variables, try {.code cb_user_missings_across()}."
    ))
  }
  if (!all(user_missing_names %in% cb$name)) {
    cli::cli_abort("{.code user_missing} contains names not found in {.code cb}.")
  }
  user_missing0 <- attr(cb, "user_missing")
  n_shared <- sum(user_missing_names %in% user_missing0)
  if (n_shared) {
    cli::cli_warn(c(
      "!" = "Existing user missing values will be overwritten for {n_shared} variable{?s}."
    ))
  }
  if (match_type) {
    data <- attr(cb, "data")
    user_missing <- lapply(setNames(nm = names(user_missing)), \(nm) {
      as_named(user_missing[[nm]], check_num_chr(data[[nm]]))
    })
  }
  keep_prev <- user_missing0[!(names(user_missing0) %in% user_missing_names)]
  user_missing <- c(user_missing, keep_prev)
  set_attrs(cb, user_missing = user_missing)
}

cb_user_missings_across <- function(cb, 
                                     user_missing, 
                                     vars = tidyselect::where(is_num_chr),
                                     match_type = TRUE) {
  data <- attr(cb, "data")
  vars <- setNames(nm = untidyselect(data, {{ vars }}))
  user_missing_list <- lapply(vars, \(x) user_missing)
  cb_user_missings_by_var(cb, user_missing = user_missing_list)
}

cb_add_lookups <- function(cb, sep1, sep2) {
  data <- attr(cb, "data")
  val_labels <- na.omit(setNames(cb$value_labels, cb$name))
  types <- sapply(data[names(val_labels)], check_num_chr)
  by_label <- lookups_from_string(
    val_labels, types = types, sep1 = sep1, sep2 = sep2
  )
  by_value <- lapply(by_label, \(x) setNames(names(x), x))
  set_attrs(cb, vals_by_label = by_label, labs_by_value = by_value)
}

reconcile_missing_labels <- function(val_labs, 
                                     missings, 
                                     conflict = c("value_label", "missing_label")) {
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
  if (conflict == "value_label") {
    names(missings)[mismatch] <- lab_name[mismatch]
  } else if (conflict == "missing_label") {
    names(val_labs)[match(lab_val[mismatch], val_labs)] <- miss_name[mismatch]
  }
  list(val_labs = val_labs, missings = missings)
}

cb_label_data <- function(cb, conflict = c("value_label", "missing_label")) {
  conflict <- match.arg(conflict)
  data <- attr(cb, "data")
  vals_by_label <- attr(cb, "vals_by_label")
  user_missing <- attr(cb, "user_missing")
  label_vars <- union(names(vals_by_label), names(user_missing))
  for (nm in label_vars) {
    val_labs <- vals_by_label[[nm]]
    missings <- user_missing[[nm]]
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
    } else {
      missings <- sort(missings)
      val_labs <- sort(val_labs)
    }
    data[[nm]] <- haven::labelled_spss(
      data[[nm]], labels = val_labs, na_values = missings
    )
  }
  set_attrs(cb, data_labelled = data)
}

cb_zap_data <- function(cb) {
  data <- attr(cb, "data_labelled")
  data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(has_val_labels),
      \(x) labelled::to_factor(x, user_na_to_na = TRUE, sort_levels = "values")
    )) |>
    haven::zap_missing() |>
    haven::zap_labels()
  set_attrs(cb, data_zapped = data)
}

string_from_lookups <- function(lookups) {
  sapply(lookups, \(x) {
    if (is.null(x)) return(NA_character_)
    nms <- names(x)
    if (is.null(nms)) labs <- "" 
    else labs <- stringr::str_c(" ", tidyr::replace_na(names(x), ""))
    stringr::str_c(glue_chr("[{x}]{labs}"), collapse = "; ")
  })
}

cb_add_val_labels <- function(cb, separate_missings = c("if_any", "yes", "no")) {
  separate_missings <- match.arg(separate_missings)

  data <- attr(cb, "data_labelled")[cb$name]
  val_labs <- labelled::val_labels(data)
  missings <- labelled::na_values(data)
  separate_missings <- separate_missings == "yes" ||
    (separate_missings == "if_any" & !rlang::is_empty(missings))

  if (separate_missings) {
    val_labs <- mapply(\(v, m) v[!(v %in% m)], v = val_labs, m = missings)
    missings <- string_from_lookups(missings)
  } else {
    missings <- NULL
  }
  val_labs <- string_from_lookups(val_labs)
  dplyr::mutate(cb, value_labels = val_labs, user_missings = missings)
}

## should maybe generalize this pattern of [get attr data] -> [sort by cb$name] -> [sapply fx]
cb_add_types <- function(cb) {
  data <- attr(cb, "data_zapped")[cb$name]
  dplyr::mutate(cb, type = sapply(data, class_collapse))
}

cb_add_missing <- function(cb) {
  data <- attr(cb, "data_zapped")[cb$name]
  dplyr::mutate(cb, missing = sapply(data, \(x) mean(is.na(x))))
}
