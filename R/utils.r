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

to_fct_chr <- function(x, levels, detail_missing, na_label) {
  x |>
    labelled::to_factor(
      levels = levels, user_na_to_na = !detail_missing,
      explicit_tagged_na = detail_missing
    ) |>
    as.character() |>
    tidyr::replace_na(na_label)
}

names_if_any <- function(x) dplyr::na_if(names(x) %||% NA_character_, "")

expand_dt <- function(dt, ...) {
  grid <- data.table::CJ(...)
  grid[, as.list(dt), by = grid]
}

repeats_to_blank <- function(x, replace = c("", "NA")) {
  replace <- if (match.arg(replace) == "NA") NA else ""
  dplyr::if_else(x == dplyr::lag(x, default = replace), replace, x)
}

user_missing_match_type <- function(nm, data) {
  classes <- class(data[[nm]])
  classes_diff <- setdiff(
    classes, c("ordered", "haven_labelled", "haven_labelled_spss", "vctrs_vctr")
  )
  if (length(classes_diff) != 1) {
    cli::cli_abort(c(
      "!" = "User missing values are not currently supported for variables of this class.",
      "i" = "Variable {.var {nm}} with class {.cls {classes}}"
    ))
  }
  if (classes_diff == "factor") "character" else classes_diff
}

as_named <- function(x, class) setNames(as(x, class), names(x))

val_labels_valid <- function(x, prefixed = FALSE) {
  UseMethod("val_labels_valid")
}

#' @export
val_labels_valid.haven_labelled <- function(x, prefixed = FALSE) {
  out <- labelled::val_labels(x, prefixed = prefixed)
  out[!(out %in% labelled::na_values(x))]
}

#' @export
val_labels_valid.default <- function(x, prefixed = FALSE) NULL

#' @export
val_labels_valid.data.frame <- function(x, prefixed = FALSE) {
  lapply(x, val_labels_valid, prefixed = prefixed)
}

val_lookups <- function(x, prefixed = FALSE) UseMethod("val_lookups")

#' @export
val_lookups.default <- function(x, prefixed = FALSE) NULL

#' @export
val_lookups.haven_labelled <- function(x, prefixed = FALSE) {
  out <- labelled::val_labels(x, prefixed = prefixed)
  if (!is.null(out)) setNames(names(out), out) else NULL
}

#' @export
val_lookups.data.frame <- function(x, prefixed = FALSE) {
  lapply(x, val_lookups, prefixed = prefixed)
}

get_value_lookups <- val_lookups

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

remove_user_na_spec <- function(x, ...) {
  labelled::set_na_values(x, setdiff(labelled::na_values(x), ...))
}

c_labels <- function(x, y, conflict = c("error", "warn", "ignore")) {
  conflict <- match.arg(conflict)
  withCallingHandlers(
    unique(c(x, y)),
    warning = function(w) {
      msg_test <- grepl("value labels", w$message, ignore.case = TRUE)
      if (msg_test && inherits(w, "warning") && conflict != "warn") {
        msg <- "{.code ..1} and {.code ..2} have conflicting value labels."
        if (conflict == "error") cli::cli_abort(msg)
        rlang::cnd_muffle(w)
      }
    }
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

