as_codebook <- function(x, 
                        name = name, 
                        label = label,
                        value_labels = value_labels,
                        type = NULL,
                        missing = NULL,
                        .keep = tidyselect::everything(),
                        .drop = NULL,
                        .user_missing = NULL,
                        .lookups = NULL, 
                        .miss_propagate = NULL,
                        .n_obs = NULL) {
  x_class <- class(x)
  if (!("data.frame" %in% x_class)) {
    cli::cli_abort(c(
      "!" = "{.code x} must be a data frame or data frame extension.",
      "i" = "{.code x} is of class {x_class}."
    ))
  }
  if (!missing(.drop)) {
    if (!missing(.keep)) {
      cli::cli_abort(c(
        "x" = "Only one of {.code .keep} and {.code .drop} should be specified."
      ))
    }
    .keep <- rlang::quo(!{{ .drop }})
  }
  class(x) <- c("li_codebook", x_class)
  x |>
    dplyr::select(
      name = {{ name }}, type = {{ type }}, label = {{ label }}, 
      value_labels = {{ value_labels }}, missing = {{ missing }},
      {{ .keep }}
    ) |> 
    set_attrs(
      user_missing = .user_missing, lookups = .lookups, 
      miss_propagate = .miss_propagate, n_obs = .n_obs
    )
}
codebook <- function(name,
                      label = NA_character_,
                      value_labels = NA_character_,
                      ..., 
                      type = NULL,
                      missing = NULL,
                      .user_missing = NULL, 
                      .lookups = NULL, 
                      .miss_propagate = NULL,
                      .n_obs = NULL) {
  tibble::tibble(
      name = name, type = type, label = label, value_labels = value_labels, 
      missing = missing, ...
    ) |>
    as_codebook(
      .user_missing = .user_missing, .lookups = .lookups, 
      .miss_propagate = .miss_propagate, .n_obs = .n_obs
    )
}
is_codebook <- function(x) "li_codebook" %in% class(x)

lookups_from_string <- function(val_labels, names, sep1, sep2) {
  fx_inner <- function(x, nm, sep1) {
    if (length(x) == 1 && is.na(x)) return(NA)
    if (!all(stringr::str_detect(x, sep1))) {
      # vals_print <- stringr::str_c('"', stringr::str_c(x, collapse = '"; "'), '"')
      cli::cli_abort(c(
        "!" = "Failed to parse value labels for {.code {nm}}"
        # "i" = "Value strings: {vals_print}"
      ))
    }
    x <- stringr::str_split(x, sep1, simplify = TRUE)
    vals <- as.numeric(x[, 1])
    labs <- x[, 2]
    haven::labelled_spss(vals, setNames(vals, labs))
  }
  num_val <- "-?\\d{1,99}"
  sep2 <- glue_chr("{sep2}(?={num_val}{sep1})")
  sep1 <- glue_chr("(?<=^{num_val}){sep1}")
  names <- names[!is.na(val_labels)]
  val_labels <- val_labels[!is.na(val_labels)]
  lookups <- stringr::str_split(val_labels, sep2) |>
    purrr::map2(names, fx_inner, sep1) |>
    setNames(names)
}

string_from_lookups <- function(lookups, names, sep1 = " = ", sep2 = "; ") {
  lookups[names] |> 
    unname() |>
    lapply(labelled::val_labels) |> 
    sapply(\(x) {
      if (is.null(x)) return(NA_character_)
      stringr::str_c(x, names(x), sep = sep1, collapse = sep2)
    })
}

# meta_select_fields <- function(meta,
#                               name = name,
#                               label = label,
#                               value_labels = value_labels,
#                               ...,
#                               type = NULL) {
#   meta |>
#     dplyr::select(
#       name = {{ name }},
#       type = {{ type }},
#       label = {{ label }},
#       value_labels = {{ value_labels }},
#       ...
#     )
# }
meta_clean_fields <- function(meta, rmv_html = !name, rmv_line_breaks = !name) {
  meta |>
    dplyr::mutate(
      dplyr::across({{ rmv_html }}, strip_html),
      dplyr::across({{ rmv_line_breaks }}, strip_line_breaks)
    )
}
meta_add_lookups <- function(meta, sep1, sep2) {
  attr(meta, "lookups") <- lookups_from_string(
    meta$value_labels, meta$name,
    sep1 = sep1, sep2 = sep2
  )
  meta
}
# meta_parse_val_labels <- function(meta,
#                               sep1,
#                               sep2,
#                               sep1_out = " = ",
#                               sep2_out = "; ") {
#   lookups <- lookups_from_string(
#     meta$value_labels, meta$name,
#     sep1 = sep1, sep2 = sep2
#   )
#   meta$value_labels <- string_from_lookups(
#     lookups, meta$name,
#     sep1 = sep1_out, sep2 = sep2_out
#   ) |>
#     unname()

#   attr(meta, "lookups") <- lookups
#   meta
# }

cb_var_attributes <- c("lookups", "user_missing", "miss_propagate")

bind_attrs <- function(which, objs) {
  attrs <- lapply(objs, attr, which)
  out <- unlist(attrs, recursive = FALSE)
  dups <- duplicated(names(out))
  if (any(dups)) {
    cli::cli_abort(c(
      "!" = "Values for attribute {.code {which}} are not unique across codebooks.",
      "i" = "Duplicated values: {toString(names(out)[dups])}"
    ))
  }
  out
}

cb_bind_rows <- function(..., .id = NULL) {
  out <- dplyr::bind_rows(..., .id = .id)
  dots <- list(...)
  if (length(dots) == 1 && rlang::is_bare_list(dots[[1]])) {
      dots <- dots[[1]]
  }
  cbs <- purrr::keep(dots, is_codebook)
  attr_out <- lapply(setNames(nm = cb_var_attributes), bind_attrs, objs = cbs)
  set_attrs(out, !!!attr_out)
}

meta_parse_complete_rc <- function(meta, sep1_out = " = ", sep2_out = "; ") {
  form <- na.omit(unique(meta$form))
  name <- stringr::str_c(form, "_complete")
  label <- stringr::str_c(form, " completion status")
  lookups <- c(Incomplete = 0, Unverified = 1, Complete = 2)
  lookups <- haven::labelled_spss(unname(lookups), labels = lookups) |>
    list() |>
    rep(length(form)) |>
    setNames(name)
  out <- codebook(name = name, label = label, .lookups = lookups)
  cb_bind_rows(out, meta)
}


meta_expand_checkbox_rc <- function(name,
                                  label,
                                  .lookups,
                                  ...,
                                  datanames) {
# could implement option to have separate "stem" and "option" fields in codebook
# for these kinds of variables
vars <- datanames[stringr::str_starts(datanames, stringr::str_c(name, "___"))]
vals <- vars |>
  stringr::str_extract("(?<=___).+") |>
  stringr::str_replace("_", "-") |>
  setNames(vars)
labs <- val_lookups(.lookups)[vals]
label_out <- stringr::str_c(label, labs, sep = " - ")
lookups_out <- setNames(rep(list(.lookups), length(vars)), vars)
codebook(
  name = vars,
  label = label_out,
  ...,
  .lookups = lookups_out
)
}

meta_parse_checkbox_rc <- function(name,
                                  label,
                                  .lookups,
                                  .user_missing = NULL,
                                  ...,
                                  datanames,
                                  sep1_out,
                                  sep2_out,
                                  label_vals = c("none", "yes_no", "meta")) {
  recode_checkbox <- function(val, lab_0, lab_1, .lookups, .user_missing) {
    vals_missing <- setdiff(.user_missing, val)
    vals_01 <- setNames(0:1, c(lab_0, lab_1))
    out <- .lookups[.lookups %in% c(0, 1, vals_missing)] |>
      labelled::drop_unused_value_labels() |>
      labelled::add_value_labels(vals_01) |>
      labelled::set_na_values(vals_missing)
  }
  # could implement option to have separate "stem" and "option" fields in codebook
  # for these kinds of variables
  label_vals <- match.arg(label_vals)
  vars <- datanames[stringr::str_starts(datanames, stringr::str_c(name, "___"))]
  vals <- vars |>
    stringr::str_extract("(?<=___).+") |>
    stringr::str_replace("_", "-") |>
    setNames(vars)
  labs <- val_lookups(.lookups)[vals]
  miss_idx <- vals %in% .user_missing

  label_out <- stringr::str_c(label, labs, sep = " - ")

  if (label_vals == "none") {
    if (any(miss_idx)) {
      cli::cli_abort(c(
        "!" = "Failed to process checkbox values for {.code {name}}.",
        "i" = '{.code label_vals = "none"} is not supported when value labels include a user-missing value.'
      ))
    }
    lookups_out <- NA
  } else {
    lab_0 <- if (label_vals == "yes_no") "No" else "Not selected"
    lab_1 <- if (label_vals == "yes_no") "Yes" else labs
    lookups_out <- mapply(
      recode_checkbox,
      vals, lab_0 = lab_0, lab_1 = lab_1,
      MoreArgs = list(.lookups = .lookups, .user_missing = .user_missing),
      SIMPLIFY = FALSE
    )
  }
  
  user_missing_out <- miss_propagate <- NULL
  if (!is.null(.user_missing)) {
    user_missing_out <- vals |> 
      lapply(\(x) remove_user_na_spec(setdiff(.user_missing, x), x)) |> 
      setNames(vars)
  }
  if (any(miss_idx)) {
    vals_pr <- if (all(is.numeric(.user_missing))) as.numeric(vals) else vals
    miss_propagate <- mapply(
      \(flag, val) list(flag = flag, val = val, vars = setdiff(vars, flag)),
      vars[miss_idx], vals_pr[miss_idx],
      SIMPLIFY = FALSE
    )
  }
  out <- codebook(
      name = vars,
      label = label_out,
      ...,
      .lookups = lookups_out,
      .user_missing = user_missing_out,
      .miss_propagate = miss_propagate
  )
}

meta_parse_checkboxes_rc <- function(meta,
                                      datanames,
                                      user_missing = NULL,
                                    #  sep1_out = " = ",
                                    #  sep2_out = "; ",
                                      label_vals = c("none", "yes_no", "meta")) {
  meta |>
    dplyr::mutate(
      .lookups = attr(meta, "lookups")[name],
      .user_missing = attr(meta, "user_missing")[name]
    ) |>
    dplyr::filter(rc_type == "checkbox") |>
    dplyr::select(!value_labels) |>
    purrr::pmap(
      meta_parse_checkbox_rc,
      datanames = datanames, 
      # sep1_out = sep1_out, sep2_out = sep2_out, 
      label_vals = label_vals
    ) |>
    cb_bind_rows() |> 
    cb_bind_rows(meta)
}

set_user_missings_by_var <- function(x, 
                                    user_missing = list(), 
                                    conflict = c("error", "warn", "ignore")) {
stopifnot(is_codebook(x))
user_missing_names <- names(user_missing)
if (!rlang::is_bare_list(user_missing) || is.null(user_missing_names)) {
  cli::cli_abort(c(
    "x" = "{.code user_missing} must be a named list",
    "i" = "To set the same missing vals across multiple variables, try {.code meta_set_user_missings_across()}."
  ))
}
if (!all(user_missing_names %in% x$name)) {
  cli::cli_abort(c(
    "x" = "{.code user_missing} contains names not found in {.code x}."
  ))
}
user_missing0 <- attr(x, "user_missing")
n_shared <- sum(user_missing_names %in% user_missing0)
if (n_shared) {
  cli::cli_warn(c(
    "!" = "Existing user missing values will be overwritten for {n_shared} variable{?s}."
  ))
}
user_missing <- user_missing |>
  lapply(\(x) {
    labels <- if (!is.null(names(x))) x else NULL
    vals <- unname(x)
    haven::labelled_spss(vals, labels = labels, na_values = vals)
  })
lookups <- attr(x, "lookups")
lookups_common_names <- intersect(names(lookups), user_missing_names)
lookups[lookups_common_names] <- lapply(lookups_common_names, \(nm) {
  out <- c_labels(lookups[[nm]], user_missing[[nm]], conflict = conflict)
  labelled::set_na_values(out, user_missing[[nm]])
})
keep_prev <- user_missing0[!(names(user_missing0) %in% user_missing_names)]
user_missing <- c(user_missing, keep_prev)
set_attrs(x, lookups = lookups, user_missing = user_missing)
}


set_user_missings_across <- function(x, user_missing, vars = tidyselect::everything()) {
  nm_df <- tibble::as_tibble(setNames(nm = as.list(x$name)))
  user_missing_list <- setNames(nm = names(dplyr::select(nm_df, {{ vars }}))) |>
    lapply(\(x) user_missing)
  set_user_missings_by_var(x, user_missing = user_missing_list)
}

process_metadata <- function(.meta,
                              name = name,
                              label = label,
                              value_labels = value_labels,
                              ...,
                              type = NULL,
                              missing = NULL,
                              .sep1,
                              .sep2,
                              .sep1_out = " = ",
                              .sep2_out = "; ",
                              .rmv_html = !name,
                              .rmv_line_breaks = !name) {
.keep <- rlang::expr(c(!!!rlang::enquos(...)))
out <- .meta |>
  as_codebook(
    name = {{ name }},
    type = {{ type }},
    label = {{ label }},
    value_labels = {{ value_labels }},
    .keep = !!.keep
  ) |>
  meta_clean_fields(
    rmv_html = {{ .rmv_html }}, rmv_line_breaks = {{ .rmv_line_breaks }}
  ) |>
  meta_add_lookups(sep1 = .sep1, sep2 = .sep2)
  out
}

propagate_user_missing_checkboxes_rc <- function(data, codebook) {
  mp <- attr(codebook, "miss_propagate")
  for (mpi in mp) {
    for (var in mpi$vars) {
      data[[var]][data[[mpi$flag]] == 1] <- mpi$val
    }
  }
  data
}


inline_hist2 <- function(x, max_bins = 8) UseMethod("inline_hist2")
inline_hist2.default <- function(x, max_bins = 8) " "
inline_hist2.numeric <- function(x, max_bins = 8) {
  skimr::inline_hist(x, n_bins = max_bins)
  ## alternative approach, where n_bins is constrained by n of unique values
  # nvals <- dplyr::n_distinct(x, na.rm = TRUE)
  # if (nvals == 0) return(" ")
  # if (nvals == 1) return("▇")
  # skimr::inline_hist(x, n_bins = pmin(nvals, max_bins))
}
inline_hist2.factor <- function(x, max_bins = 8) {
  tbl <- table(x)
  nvals <- length(tbl)
  if (nvals == 0 || nvals > max_bins) return(" ")
  if (nvals == 1 && tbl == 0) return("▁") 
  if (nvals == 1) return("▇")
  nvals <- dplyr::n_distinct(x, na.rm = TRUE)
  if (nvals == 0) return(" ")
  if (nvals == 1) return("▇")
  skimr:::spark_bar(tbl / max(tbl))
}

generate_codebook <- function(data,
                              metadata,
                              add_type = !("type" %in% names(metadata))) {
  # dat_user_missings <- user_missing_to_na(dat, user_missing)
  datnames <- names(data)
  attrs <- attributes(metadata)
  attrs <- attrs[names(attrs) %in% cb_var_attributes]
  attrs <- lapply(attrs, \(att) {
      out <- att[datnames]
      out[!sapply(out, is.null)]
    }) |> 
    c(n_obs = nrow(data))

  out <- tibble::tibble(name = datnames) |>
    dplyr::left_join(metadata, dplyr::join_by(name)) |>
    as_codebook() |>
    set_attrs(!!!attrs)
  out$value_labels <- string_from_lookups(attr(out, "lookups"), out$name)
  
  if (add_type) {
    out <- out |>
      dplyr::mutate(
        type = ifelse(!is.na(value_labels), "categorical", sapply(data, class_collapse)),
      .after = name
    )
  }    
  # also convert to factor here
  data_rmv_missing <- data |>
    propagate_user_missing_checkboxes_rc(cb) |>
    user_missing_to_na(cb)
  
  # hists <- data_rmv_missing |>
  #   dplyr::summarize(
  #     dplyr::across(tidyselect::where(is.numeric), inline_hist2)
  #   ) |>
  #   tidyr::pivot_longer(tidyselect::everything(), values_to = "hist")
  
  out |>
    dplyr::mutate(
      missing = sapply(data_rmv_missing, \(x) mean(is.na(x))),
      hist = sapply(data_rmv_missing, inline_hist2)
    ) |> 
    dplyr::relocate(name, type, label, value_labels, missing, hist)
}

write_codebook_header <- function(dataset_name, 
                                  incl_date, 
                                  incl_dims, 
                                  nrows, 
                                  ncols) {
  cb_name <- cb_dims <- cb_date <- NULL
  if (!is.null(dataset_name)) cb_name <- glue_chr("Dataset: {dataset_name}")
  if (incl_dims) {
    cb_dims <- cli::pluralize("{nrows} record{?s} x {ncols} variable{?s}")
  }
  cb_date <- if (incl_date) glue_chr("Codebook generated {Sys.Date()}")
  c(dataset_name, cb_dims, cb_date)
}
write_codebook <- function(x, 
                            file, 
                            dataset_name = NULL, 
                            incl_date = TRUE, 
                            incl_dims = TRUE,
                            format_names = TRUE,
                            overwrite = TRUE) {
  nrows <- nrow(x)
  info <- write_codebook_header(
    dataset_name = dataset_name, incl_date = incl_date, incl_dims = incl_dims,
    nrows = attr(x, "n_obs"), ncols = nrow(x)
  )
  cols <- list(all = seq_along(x), missing = which(names(x) == "missing"))
  rows <- tibble::lst(
    dat_start = length(info) + 1,
    info = seq_along(info),
    dat = seq_len(nrows) + dat_start,
    all = seq_len(nrows + dat_start),
    banded = dat[seq(1, nrows, by = 2)]
  )
  styles <- list(
    header = openxlsx::createStyle(
      textDecoration = "italic", border = c("top", "bottom"),
      borderColour = c("#808080", "black"), borderStyle = c("thin", "double")
    ),
    bold = openxlsx::createStyle(textDecoration = "bold"),
    all = openxlsx::createStyle(fgFill = "white", fontName = "Aptos Narrow"),
    gray = openxlsx::createStyle(fgFill = "#EEEEEE"),
    pct = openxlsx::createStyle(numFmt = "0.0%")
  )
  wb <- openxlsx::createWorkbook()
  cb <- "Codebook"
  openxlsx::addWorksheet(wb, cb)
  if (!is.null(info)) {
    openxlsx::writeData(wb, cb, info)
    openxlsx::addStyle(wb, cb, styles$bold, rows = rows$info, cols = 1)
  }
  if (format_names) {
    names(x) <- stringr::str_to_title(
      stringr::str_replace_all(names(x), "_", " ")
    )
  }
  openxlsx::writeData(
    wb, cb, x, headerStyle = styles$header, startRow = rows$dat_start
  )
  openxlsx::addStyle(
    wb, cb, styles$all, rows = rows$all, cols = cols$all, gridExpand = TRUE, 
    stack = TRUE
  )
  openxlsx::addStyle(
    wb, cb, styles$gray, rows = rows$banded, cols = cols$all, 
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, cb, styles$pct, rows = rows$dat, cols = cols$missing, 
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::setColWidths(wb, cb, cols = cols$all, widths = "auto")
  openxlsx::freezePane(
    wb, cb, firstActiveRow = rows$dat_start + 1, firstActiveCol = 2
  )

  ok <- openxlsx::saveWorkbook(
    wb, file, overwrite = overwrite, returnValue = TRUE
  )
  if (!ok) cli::cli_abort("Failed to save codebook")
  invisible(file)
}




label_data <- function(data, codebook) {
  lookups <- attr(codebook, "lookups")
  user_miss_unique <- user_missing[!(names(user_missing) %in% names(lookups))]
  labs <- c(lookups, user_miss_unique)
  labs <- lookups
  for (nm in names(labs)) {
    data[[nm]] <- labelled::copy_labels(labs[[nm]], data[[nm]])
  }
  data
}


user_missing_to_na <- function(dat, codebook) {
  user_missing <- attr(codebook, "user_missing")
  for (var in names(user_missing)) {
    dat[[var]][dat[[var]] %in% user_missing[[var]]] <- NA
  }
  dat
}

categorical_to_factor <- function(dat, codebook) {
  attr(codebook, "lookups")
  for (var in names(user_missing)) {
    dat[[var]][dat[[var]] %in% user_missing[[var]]] <- NA
  }
  dat
}

