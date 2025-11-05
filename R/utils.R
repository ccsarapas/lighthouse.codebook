class_collapse <- function(x, sep = ", ") stringr::str_c(class(x), collapse = sep)
set_attrs <- function(x, ...) {
  dots <- rlang::list2(...)
  for (nm in names(dots)) attr(x, nm) <- dots[[nm]]
  x
}
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

labelled_cast <- function(x, to) {
  stopifnot("haven_labelled" %in% class(x))
  if (!(rlang::is_bare_character(to) || rlang::is_bare_numeric(to))) {
    cli::cli_abort(
      "Only casting between base character and numeric types is supported."
    )
  }
  type_x <- typeof(x)
  type_to <- typeof(to)
  if (type_x == type_to) return(x)
  attr_x <- attributes(x)
  for (att in intersect(names(attr_x), c("labels", "na_values"))) {
    nms <- names(attr_x[[att]])
    attr_x[[att]] <- as(attr_x[[att]], type_to)
    names(attr_x[[att]]) <- nms
  }
  attr_x$class[attr_x$class == type_x] <- type_to
  out <- as(unclass(x), type_to)
  attributes(out) <- attr_x
  out
}

retype_label <- function(x, 
                         to, 
                         not_found = c("error", "drop", "keep"), 
                         bad_type = c("error", "drop", "keep")) {
  err_drop_keep <- function(x, msg, do = c("error", "drop", "keep")) {
    switch(match.arg(do), error = cli::cli_abort(msg), drop = NULL, keep = x)
  }
  if (is.null(to)) {
    return(err_drop_keep(
      x, "Variable {.code {nm}} not found in {.code data}", not_found
    ))
  }
  if (!(is.character(to) || is.numeric(to))) {
    return(err_drop_keep(
      x, "Can't cast {.code {nm}} to {class(to)}", bad_type
    ))
  }
  labelled_cast(x, to)
}

retype_labels <- function(codebook,
                          data,
                          not_found = c("error", "drop", "keep"),
                          bad_type = c("error", "drop", "keep")) {
  lookups <- attr(codebook, "lookups")
  user_missing <- attr(codebook, "user_missing")
  lookups <- names(lookups) |>
    lapply(\(nm) retype_label(lookups[[nm]], data[[nm]]))
  user_missing <- names(user_missing) |>
    lapply(\(nm) retype_label(user_missing[[nm]], data[[nm]]))
  user_missing
}
val_lookups <- function(x, prefixed = FALSE) UseMethod("val_lookups")
val_lookups.default <- function(x, prefixed = FALSE) NULL
val_lookups.haven_labelled <- function(x, prefixed = FALSE) {
  out <- labelled::val_labels(x, prefixed = prefixed)
  if (!is.null(out)) setNames(names(out), out) else NULL
}
val_lookups.data.frame <- function(x, prefixed = FALSE) {
  lapply(x, val_lookups, prefixed = prefixed)
}
get_value_lookups <- val_lookups
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