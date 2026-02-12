#' Generate a codebook object
#'
#' @description
#' `cb_create()` builds an object of class `"li_codebook"` from a dataset and optional
#' metadata. The resulting object can be used to write an Excel workbook with variable
#' and data summaries (using [`cb_write()`]), extract processed data ([`cb_get_data()`]),
#' or generate dataset summaries ([`cb_summarize_numeric()`], [`cb_summarize_categorical()`],
#' [`cb_summarize_text()`]).
#'
#' @param data A data frame.
#' @param metadata A data frame containing metadata, such as variable labels and value
#' labels.
#' @param ... Additional columns from `metadata` to preserve in the final codebook.
#'   New names can be assigned by passing named arguments. Columns for variable
#'   name, form, variable label, and value labels are included by default.
#' @param .name,.var_label,.val_labels Columns in `metadata` containing variable
#'   name, variable label, and value labels, respectively. If `metadata` is provided,
#'   `.name` must be specified. `.var_label` and `.val_labels` may be set to `NULL`
#'   to omit.
#' @param .user_missing A formula or list of formulas specifying user missing values.
#'   Formulas should specify variables on the left-hand side (as variable names
#'   or [tidyselect][dplyr_tidy_select] expressions), and missing values on the
#'   right-hand side. If left-hand side is omitted, defaults to `tidyselect::everything()`.
#'   See "Specifying user missing values" below for examples.
#' @param .split_var_labels A [`tidyselect`][dplyr_tidy_select] expression or list of tidyselect
#'   expressions, indicating (sets of) variable labels with a common stem that should
#'   be extracted into a separate column.
#' @param .include_r_classes Include a column listing class(es) of each variable?
#'   (e.g., `"factor"`, `"POSIXct, POSIXt"`.)
#' @param .include_types Include a column listing simplified type for each variable?
#'   (e.g,. `"categorical"`, `"date-time"`.)
#' @param .val_labs_sep1,.val_labs_sep2 Regex patterns separating value labels
#'   in `metadata`. `.val_labs_sep1` separates values from labels, and `.val_labs_sep2`
#'   separates value/label pairs. e.g., if value labels are in format `"1, First label|2, Second label"`,
#'   set `.val_labs_sep1` to `","` and `.val_labs_sep2` to `"\\|"`.
#' @param .rmv_html Should HTML tags be removed from metadata (e.g., from variable
#'   and value labels)?
#' @param .rmv_line_breaks Should line breaks be removed from metadata (e.g., from
#'   variable and value labels)? If `TRUE`, line breaks will be replaced with `" / "`.
#' @param .user_missing_col Include value labels for user missing values in a separate
#'   column? The default, `"if_any"`, adds the column only if user missings are
#'   specified for at least one variable.
#' @param .user_missing_conflict If different labels for a value are provided in
#'   metadata and user missings, which should be used?
#' @param .user_missing_incompatible How to handle variables specified in `.user_missing`
#'   that aren't compatible with user missing values (e.g., logical, Date, or POSIXt)?
#'
#' @return
#' An `"li_codebook"` object, consisting of (1) a tibble summarizing the passed
#' dataset and (2) attributes containing the passed dataset (in several formats)
#' and additional metadata. Specifically:
#' - A tibble with columns:
#'     - `name`: variable name
#'     - `type`: optional column containing simplified variable type
#'     - `class`: optional column containing class(es) of each variable
#'     - `label_stem`: optional column containing variable label stems, if any variables
#'       are specified in `.split_var_labels`
#'     - `label`: variable label
#'     - `values`: values, with labels if applicable
#'     - `user_missing`: optional column, depending on value of `.user_missing_col`,
#'        showing user missing values, with labels if applicable
#'     - `missing`: proportion missing
#'     - additional columns if specified in `...`
#' - Attributes:
#'     - Transformed versions of the passed dataset. See [`cb_get_data()`]
#'     - Lookup tables and other metadata used internally.
#'
#' @section Specifying user missing values:
#' User missing values are defined by passing a formula or list of formulas to the
#' `.user_missing` argument. Formulas should specify variables on the left-hand
#' side and user missing values for those variables on the right-hand side:
#' \preformatted{
#' cb <- cb_create(data, metadata, .user_missing = var1 ~ 99)
#' }
#' The same user missings can be applied to multiple variables using [tidyselect][dplyr_tidy_select]
#' expressions.
#' \preformatted{
#' # for variables `var1` through `var5`
#' .user_missing = var1:var5 ~ 99
#'
#' # for all numeric variables, plus `var6` and `var7`
#' .user_missing = c(where(is.numeric), var6, var7) ~ c(-9, -8, -7)
#'
#' # omitted left-hand side defaults to `tidyselect::everything()`
#' .user_missing = ~ -99
#' }
#' Different user missings can be applied to different variables using a list of
#' formulas:
#' \preformatted{
#' .user_missing = list(
#'   starts_with("status") ~ c(98, 99),
#'   var7:var10 ~ 97
#' )
#' }
#' User missing values may optionally be named to set value labels:
#' \preformatted{
#' .user_missing = ~ c(Declined = -98, "Not applicable" = -99)
#' }
#' If labels set in `.user_missing` conflict with those in `metadata`, `.user_missing_conflict`
#' controls which labels are used.
#'
#' User missings may be set for numeric, character, factor/ordered factor, and haven_labelled/haven_labelled_spss
#' vectors. For factors, user missings are set based on factor labels (not the underlying
#' integer codes). For `"haven_labelled"` vectors, user missings are set based on
#' values (not value labels). By default, variables with incompatible classes (e.g.,
#' logical, Date, POSIXt) will be ignored if specified in `.user_missing`. This
#' behavior can be changed using the `.user_missing_incompatible` argument.
#'
#' @examples
#' diamonds2 <- ggplot2::diamonds |>
#'   transform(
#'     carat_group = as.integer(cut(carat, breaks = 3, labels = 1:3)),
#'     price_group = as.integer(cut(
#'       price,
#'       breaks = c(0, 500, 1000, 2000, 5000, 10000, Inf),
#'       labels = 1:6,
#'       right = FALSE
#'     ))
#'   )
#'
#' # basic codebook
#' cb_create(diamonds2)
#'
#' # convert variables to factor to treat as categorical
#' diamonds2 |>
#'   transform(
#'     carat_group = factor(carat_group),
#'     price_group = factor(price_group)
#'   ) |>
#'   cb_create()
#'
#' # provide metadata for variable and value labels
#' diamonds_meta <- data.frame(
#'   name = names(diamonds2),
#'   label = c(
#'     # from ?ggplot2::diamonds
#'     "price in US dollars ($326–$18,823)",
#'     "weight of the diamond (0.2–5.01)",
#'     "quality of the cut (Fair, Good, Very Good, Premium, Ideal)",
#'     "diamond colour, from D (best) to J (worst)",
#'     "a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))",
#'     "length in mm (0–10.74)",
#'     "width in mm (0–58.9)",
#'     "depth in mm (0–31.8)",
#'     "total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)",
#'     "width of top of diamond relative to widest point (43–95)",
#'     "diamond carat (3 groups)",
#'     "diamond price (6 groups)"
#'   ),
#'   val_labels = c(
#'     rep(NA, 10),
#'     "1 = small; 2 = medium; 3 = large",
#'     "1 = <$500; 2 = $500-$999; 3 = $1,000-$1,999; 4 = $2,000-$4,999; 5 = $5,000-$9,999; 6 = $10,000+"
#'   )
#' )
#'
#' cb_create(
#'   diamonds2, diamonds_meta,
#'   .val_labs_sep1 = " = ", .val_labs_sep2 = "; "
#' )
#' @export
cb_create <- function(data,
                      metadata = NULL,
                      ...,
                      .name = name,
                      .var_label = label,
                      .val_labels = val_labels,
                      .user_missing = NULL,
                      .split_var_labels = NULL,
                      .include_types = !.include_r_classes,
                      .include_r_classes = FALSE,
                      .val_labs_sep1 = NULL,
                      .val_labs_sep2 = NULL,
                      .rmv_html = TRUE,
                      .rmv_line_breaks = TRUE,
                      .user_missing_col = c("if_any", "yes", "no"),
                      .user_missing_conflict = c("metadata", "missing_label"),
                      .user_missing_incompatible = c("ignore", "warn", "error")
                      ) {
  data |>
    cb_init(
      metadata,
      meta_var_name = {{ .name }}, meta_var_label = {{ .var_label }},
      meta_val_labels = {{ .val_labels }}, ...
    ) |>
    cb_clean_fields(rmv_html = .rmv_html, rmv_line_breaks = .rmv_line_breaks) |>
    cb_user_missings(
      user_missing = .user_missing,
      incompatible = .user_missing_incompatible
    ) |>
    cb_add_lookups(sep1 = .val_labs_sep1, sep2 = .val_labs_sep2) |>
    cb_label_data(conflict = .user_missing_conflict) |>
    cb_zap_data() |>
    cb_add_dims() |>
    cb_add_val_labels_col(user_missing_col = .user_missing_col) |>
    cb_add_type_col(
      include_r_classes = .include_r_classes,
      include_types = .include_types
    ) |>
    cb_add_missing_col() |>
    cb_split_labels_col(split_var_labels = rlang::enexpr(.split_var_labels))
}

#' Extract data from a codebook object
#'
#' Codebook objects created by [`cb_create()`] and friends contain several transformed
#' versions of the originally passed dataset. These can be extracted using `cb_get_data()`.
#'
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param format Format of the returned data, either `"factors"` or `"haven"`; 
#'   see below for details.
#'
#' @return
#' A tibble with variables formatted based on the `format` argument.
#' - For `"factors"`, all variables with value labels are converted to factors, 
#'   and all user missings are converted to `NA`.
#' - For `"haven"`, variable labels, value labels, and user missings are encoded 
#'   using class [`"haven_labelled_spss"`][haven::labelled]`.
#' 
#' Both formats may also reflect transformations made by variants of [`cb_create()`].
#' In particular, for codebooks created using [`cb_create_redcap()`], integer coercion 
#' and propagation of user missings across checkbox variables.
#' 
#' @export
cb_get_data <- function(cb, format = c("factors", "haven")) {
  check_codebook(cb)
  tryCatch(
    format <- match.arg(format),
    error = \(e) {
      if (format == "values") {
        cli::cli_abort(
          '`format = "values"` is no longer supported.',
          call = parent.frame(4)
        )
      }
      stop(e)
    }
  )
  if (format == "factors") attr(cb, "data_zapped")
  else attr(cb, "data_labelled")
}

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
        label = {{ meta_var_label }}, values = {{ meta_val_labels }},
        ...
      )
  } else {
    out <- out |>
      dplyr::mutate(values = NA_character_)
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
  val_labels <- na.omit(setNames(cb$values, cb$name))
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
  missings <- attr(cb, "user_missing")[cb$name] |>
    setNames(cb$name)    
  user_missing_col <- user_missing_col == "yes" || (
    user_missing_col == "if_any" & !(
      rlang::is_empty(missings) || all(vapply(missings, rlang::is_empty, logical(1)))
    )
  )
  if (user_missing_col) {
    missings <- lapply(missings, try_sort_numeric)
    val_labs <- mapply(
      \(v, m) v[!(v %in% m)],
      v = val_labs, m = missings,
      SIMPLIFY = FALSE
    )
    missings <- string_from_lookups(missings)
  } else {
    missings <- NULL
  }
  val_labs <- string_from_lookups(val_labs, no_prefix = attr(cb, "factors"))
  dplyr::mutate(cb, values = val_labs, user_missings = missings)
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
cb_add_type_col <- function(cb, include_r_classes = FALSE, include_types = FALSE) {
  relabel_classes <- function(x) {
        classes <- class(x)
        if ("ordered" %in% classes) return("ordinal")
        if ("factor" %in% classes) return("categorical")
        if ("logical" %in% classes) return("boolean")
        if ("character" %in% classes) return("text")
        if ("Date" %in% classes) return("date")
        if ("POSIXt" %in% classes) return("date-time")
        if ("hms" %in% classes) return("time")
        if (any(c("difftime", "Period", "Duration", "Interval") %in% classes)) {
          return("duration")
        }
        # numeric and integer stay as is, as does any unknown type
        paste(classes, collapse = ", ")
      }
    if (include_r_classes) {
      data <- attr(cb, "data")[cb$name]
      cb <- cb |>
        dplyr::mutate(
          class = vapply(data, class_collapse, character(1)),
          .after = name
        )
    }
    if (include_types) {
      data <- attr(cb, "data_zapped")[cb$name]
      cb <- cb |>
        dplyr::mutate(
          type = vapply(data, relabel_classes, character(1)),
          .after = name
        )
    }
    cb
}

cb_add_missing_col <- function(cb) {
  data <- attr(cb, "data_zapped")[cb$name]
  dplyr::mutate(cb, missing = vapply(data, \(x) mean(is.na(x)), double(1)))
}

