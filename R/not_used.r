## not currently used, will probably remove

inline_hist2 <- function(x, max_bins = 8, ...) UseMethod("inline_hist2")
inline_hist2.default <- function(x, max_bins = 8, ...) " "
inline_hist2.numeric <- function(x, max_bins = 8, ...) {
  skimr::inline_hist(x, n_bins = max_bins)
  ## alternative approach, where n_bins is constrained by n of unique values
  # nvals <- dplyr::n_distinct(x, na.rm = TRUE)
  # if (nvals == 0) return(" ")
  # if (nvals == 1) return("▇")
  # skimr::inline_hist(x, n_bins = pmin(nvals, max_bins))
}

inline_hist2.factor <- function(x, max_bins = 8, separate = TRUE, ...) {
  tbl <- table(x)
  nvals <- length(tbl)
  if (nvals == 0 || nvals > max_bins) return(" ")
  if (nvals == 1 && tbl == 0) return("▁") 
  if (nvals == 1) return("▇")
  nvals <- dplyr::n_distinct(x, na.rm = TRUE)
  if (nvals == 0) return(" ")
  if (nvals == 1) return("▇")
  out <- skimr:::spark_bar(tbl / max(tbl))
  if (separate) {
    out <- out |> stringr::str_split_1("") |> stringr::str_c(collapse = "\U200A")
  }
  out
}

cb_add_hists <- function(cb, max_bins = 8, separate = TRUE) {
  data <- attr(cb, "data_zapped")[cb$name]
  dplyr::mutate(
    cb,
    hist = sapply(data, inline_hist2, max_bins = max_bins, separate = separate)
  )
}


retype_label <- function(x,
                         to,
                         not_found = c("error", "drop", "keep"),
                         bad_type = c("error", "drop", "keep")) {
  err_drop_keep <- function(x, msg, do = c("error", "drop", "keep")) {
    switch(match.arg(do),
      error = cli::cli_abort(msg),
      drop = NULL,
      keep = x
    )
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