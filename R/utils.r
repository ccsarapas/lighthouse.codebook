is_codebook <- function(x) "li_codebook" %in% class(x)
check_codebook <- function(x) {
  arg <- as.character(rlang::ensym(x))
  if (!is_codebook(x)) {
    cli::cli_abort('{.arg arg} must be an object of class `"li_codebook"`.')
  }
}
check_user_missing_arg <- function(x) {
  arg <- as.character(rlang::ensym(x))
  if (!(
      rlang::is_formula(x) || (is.list(x) && all(sapply(x, rlang::is_formula)))
    )) {
    cli::cli_abort("{.arg arg} must be a formula or list of formulas.")
  }
  if (rlang::is_formula(x)) x <- list(x)
  x
}
set_attrs <- function(x, ...) {
  dots <- rlang::list2(...)
  for (nm in names(dots)) attr(x, nm) <- dots[[nm]]
  x
}

sort_as_numeric <- function(x, decreasing = FALSE, ...) {
  x[order(as.numeric(x), decreasing = decreasing, ...)]
}
try_sort_numeric <- function(x,
                             decreasing = FALSE,
                             partial = c("numeric", "character"),
                             ...) {
  coercible <- lighthouse::is_coercible_numeric(x, na = "TRUE")
  if (all(coercible)) {
    sort_as_numeric(x, decreasing = decreasing, ...)
  } else if (match.arg(partial) == "numeric") {
    c(
      sort_as_numeric(x[coercible], decreasing = decreasing, ...),
      sort(x[!coercible], decreasing = decreasing, ...)
    )
  } else {
    sort(x, decreasing = decreasing, ...)
  }
}
try_sort_numeric(letters)
  coercible <- lighthouse::is_coercible_numeric(letters, na = "TRUE")

class_collapse <- function(x, sep = ", ") stringr::str_c(class(x), collapse = sep)
strip_html <- function(x) {
  stopifnot(is.character(x))
  has_tags <- grepl("<[A-Za-z!/]", x)
  x[has_tags] <- vapply(
    x[has_tags],
    \(txt) rvest::html_text2(rvest::minimal_html(txt)),
    FUN.VALUE = character(1)
  )
  x
}
strip_line_breaks <- function(x, replace = " / ", combine_multiple = TRUE) {
  lb <- if (combine_multiple) "(\n|\r)+" else "\n|\r"
  stringr::str_replace_all(x, lb, replace)
}

fct_replace_na <- function(x, replace) {
  x <- factor(x)
  if (anyNA(x)) forcats::fct_na_value_to_level(x, level = replace) else x
}

deframe_nest <- function(x) {
  if (ncol(x) == 2) return(tibble::deframe(x))
  lapply(split(x[, -1], x[, 1]), deframe_nest)
}

names_if_any <- function(x) dplyr::na_if(names(x) %||% NA_character_, "")

expand_dt <- function(dt, ...) {
  grid <- data.table::CJ(...)
  grid[, as.list(dt), by = grid]
}

repeats_to_blank <- function(x, replace = c("empty", "NA")) {
  # changes "" to "empty" because "" doesn't work with `match.arg()`
  replace <- if (match.arg(replace) == "empty") "" else NA
  dplyr::if_else(lighthouse::is_TRUE(x == dplyr::lag(x)), replace, x)
}

can_have_labels <- function(nm, data) {
  allowed <- c(
    "integer", "double", "numeric", "character", "factor", "ordered",
    "haven_labelled", "haven_labelled_spss", "vctrs_vctr"
  )
  vapply(data[nm], \(x) all(class(x) %in% allowed), logical(1))  
}

# check_can_have_labels <- function(nm, data) {
#   ok <- can_have_labels(nm, data)
#   if (!all(ok)) {
#     bad_var <- nm[!ok][[1]]
#     bad_class <- class(data[[bad_var]])
#     cli::cli_abort(c(
#       "!" = "This operation is not currently supported for variables of this class.",
#       "i" = "Variable {.var {nm}} with class {.cls {classes}}"
#     ))

# }

cb_match_type <- function(nm, 
                          data, 
                          ignore = c("ordered", "haven_labelled", "haven_labelled_spss", "vctrs_vctr")) {
  classes <- class(data[[nm]])
  classes_diff <- setdiff(classes, ignore)
  if (!can_have_labels(nm, data) || length(classes_diff) != 1) {
    cli::cli_abort(c(
      "!" = "This operation is not currently supported for variables of this class.",
      "i" = "Variable {.var {nm}} with class {.cls {classes}}"
    ))
  }
  if (classes_diff == "factor") "character" else classes_diff
}

as_named <- function(x, class) setNames(as(x, class), names(x))


has_val_labels <- function(x) !is.null(labelled::val_labels(x))

to_labelled_chr <- function(x, 
                            labels = setNames(nm = levels(x)), 
                            na_values = NULL,
                            label = NULL) {
  stopifnot(is.factor(x))
  haven::labelled_spss(
    as.character(x), labels = labels, na_values = na_values, label = label
  )
}

spread_if_any <- function(..., na.rm = TRUE) {
  lighthouse::max_if_any(..., na.rm = na.rm) - lighthouse::min_if_any(..., na.rm = na.rm)
}

#' @export
nan_to_na.default <- function(x) dplyr::if_else(is.nan(x), NA, x)

#' @export
nan_to_na.list <- function(x) lapply(x, nan_to_na)

#' @export
nan_to_na.data.frame <- function(x) {
  x[] <- lapply(x, nan_to_na)
  x
}

nan_to_na <- function(x) UseMethod("nan_to_na")
