# include as internal function
glue_chr <- lighthouse::glue_chr

# just added for now for convenience
in_excel <- lighthouse::in_excel

dataset_dir <- file.path("..", "Example LI datasets")
# gain <- haven::read_sav(file.path(dataset_dir, "GAIN", "GAIN-Quick-v4.sav"))
# gain_dict <- labelled::generate_dictionary(gain) |>
#   tibble::as_tibble()

ugh <- readRDS(file.path(dataset_dir, "UGH", "_xSample_UGH_Data.rds"))
# grouped by record_id -- is this always the case?
ugh <- dplyr::ungroup(ugh)

ugh_codebook <- readRDS(file.path(dataset_dir, "UGH", "_xSample_UGH_Codebook.rds"))


{
  class_collapse <- function(x, sep = ", ") stringr::str_c(class(x), collapse = sep)
  set_attrs <- function(x, ...) {
    dots <- rlang::list2(...)
    for (nm in names(dots)) attr(x, nm) <- dots[[nm]]
    x
  }
  missing_string <- function(dat) {
    n_miss <- sapply(dat, \(x) sum(is.na(x)))
    p_miss <- sapply(dat, \(x) mean(is.na(x)))
    sprintf("%i (%.1f%%)", n_miss, p_miss * 100)
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
  lookups_from_string <- function(val_labels, names, sep1, sep2) {
    fx_inner <- function(x, nm, sep1) {
      if (!all(stringr::str_detect(x, sep1))) {
        # vals_print <- stringr::str_c('"', stringr::str_c(x, collapse = '"; "'), '"')
        cli::cli_abort(c(
          "!" = "Failed to parse value labels for {.code {nm}}"
          # "i" = "Value strings: {vals_print}"
        ))
      }
      x <- stringr::str_split(x, sep1, simplify = TRUE)
      setNames(x[, 2], x[, 1])
    }
    num_val <- "-?\\d{1,99}"
    sep2 <- glue_chr("{sep2}(?={num_val}{sep1})")
    sep1 <- glue_chr("(?<=^{num_val}){sep1}")
    names <- names[!is.na(val_labels)]
    val_labels <- val_labels[!is.na(val_labels)]
    stringr::str_split(val_labels, sep2) |>
      purrr::map2(names, fx_inner, sep1) |>
      setNames(names)
  }
  string_from_lookups <- function(lookups, names, sep1 = " = ", sep2 = "; ") {
    lookups <- lookups |>
      sapply(\(x)stringr::str_c(names(x), x, sep = sep1, collapse = sep2))
    unname(lookups[names])
  }

  meta_select_fields <- function(meta, 
                                name = name, 
                                label = label, 
                                value_labels = value_labels, 
                                ...,
                                type = NULL) {
    meta |>
      dplyr::select(
        name = {{ name }},
        type = {{ type }},
        label = {{ label }},
        value_labels = {{ value_labels }},
        ...
      )
  }
  meta_clean_fields <- function(meta, rmv_html = !name, rmv_line_breaks = !name) {
    meta |>
      dplyr::mutate(
        dplyr::across({{ rmv_html }}, strip_html),
        dplyr::across({{ rmv_line_breaks }}, strip_line_breaks)
      )
  }
  meta_parse_val_labels <- function(meta, 
                                sep1, 
                                sep2, 
                                sep1_out = " = ", 
                                sep2_out = "; ") {
    lookups <- lookups_from_string(
      meta$value_labels, meta$name,
      sep1 = sep1, sep2 = sep2
    )
    meta$value_labels <- string_from_lookups(
      lookups, meta$name,
      sep1 = sep1_out, sep2 = sep2_out
    ) |>
      unname()
    
    attr(meta, "lookups") <- lookups
    meta
  }
  meta_parse_checkbox_rc <- function(name,
                                    label,
                                    .lookups,
                                    ...,
                                    datanames,
                                    user_missing,
                                    sep1_out,
                                    sep2_out,
                                    label_vals = c("none", "yes_no", "meta")) {
    # could implement option to have separate "stem" and "option" fields in codebook
    # for these kinds of variables
    label_vals <- match.arg(label_vals)
    vars <- datanames[stringr::str_starts(datanames, stringr::str_c(name, "___"))]
    vals <- vars |>
      stringr::str_extract("(?<=___).+") |>
      stringr::str_replace("_", "-")
    miss_idx <- vals %in% user_missing
    missings <- .lookups[miss_idx]
    if (any(miss_idx) && label_vals == "none") {
      cli::cli_abort(c(
        "!" = "Failed to process checkbox values for {.code {name}}.",
        "i" = '{.code label_vals = "none"} is not supported when value labels include a user-missing value.'
      ))
    }
    opts <- setNames(.lookups[vals], vars)
    label_out <- stringr::str_c(label, opts, sep = " - ")
    lookups_out <- switch(label_vals,
      none = NA,
      yes_no = rep(list(c("0" = "No", "1" = "Yes", missings)), length(opts)),
      meta = lapply(opts, \(opt) c("0" = "Not selected", "1" = opt, missings))
    )
    # lookups_out[miss_idx] <- lookups_out[miss_idx] |>
    #   lapply(\(x) x[setdiff(names(x), "1")])
    lookups_out[miss_idx] <- mapply(
      \(lkup, val) lkup[setdiff(names(lkup), val)],
      lookups_out[miss_idx],
      vals[miss_idx],
      SIMPLIFY = FALSE
    )
    miss_propagate <- mapply(
      \(flag, val) list(flag = flag, val = val, vars = setdiff(vars, flag)),
      vars[miss_idx], vals[miss_idx],
      SIMPLIFY = FALSE
    )
    val_labels_out <- lookups_out |>
      string_from_lookups(names = vars, sep1 = sep1_out, sep2 = sep2_out)
    out <- codebook(
        name = vars,
        label = label_out,
        value_labels = val_labels_out,
        ...,
        .lookups = lookups_out,
        .miss_propagate = miss_propagate
    )
  }

  meta_parse_checkboxes_rc <- function(meta,
                                       datanames,
                                       user_missing = NULL,
                                       sep1_out = " = ",
                                       sep2_out = "; ",
                                       label_vals = c("none", "yes_no", "meta")) {
    meta |>
      dplyr::mutate(.lookups = attr(meta, "lookups")[name]) |>
      dplyr::filter(rc_type == "checkbox") |>
      dplyr::select(!value_labels) |>
      purrr::pmap(
        meta_parse_checkbox_rc,
        datanames = datanames, user_missing = user_missing, sep1_out = sep1_out,
        sep2_out = sep2_out, label_vals = label_vals
      ) |>
      cb_bind_rows() |> 
      cb_bind_rows(meta)
  }
  cb_attributes <- c("lookups", "miss_propagate")
  
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
    attr_out <- lapply(setNames(nm = cb_attributes), bind_attrs, objs = cbs)
    set_attrs(out, !!!attr_out)
  }

  meta_parse_complete_rc <- function(meta, sep1_out = " = ", sep2_out = "; ") {
    form <- na.omit(unique(meta$form))
    name <- stringr::str_c(form, "_complete")
    label <- stringr::str_c(form, " completion status")
    lookups <- c("0" = "Incomplete", "1" = "Unverified", "2" = "Complete") |>
      list() |> 
      rep(length(form)) |>
      setNames(name)
    value_labels <- lookups |> 
      string_from_lookups(names = name, sep1 = sep1_out, sep2 = sep2_out)
    out <- codebook(name, form, label, value_labels, .lookups = lookups)
    cb_bind_rows(out, meta)
  }
  as_codebook <- function(x,
                          .lookups = NULL, 
                          .miss_propagate = NULL) {
    x_class <- class(x)
    if (!("data.frame" %in% x_class)) {
      cli::cli_abort(c(
        "!" = "{.code x} must be a data frame or data frame extension.",
        "i" = "{.code x} is of class {x_class}."
      ))
    }
    class(x) <- c("li_codebook", x_class)
    set_attrs(x, lookups = .lookups, miss_propagate = .miss_propagate)
  }
  is_codebook <- function(x) "li_codebook" %in% class(x)
  codebook <- function(...,  .lookups = NULL, .miss_propagate = NULL) {
    tibble::tibble(...) |>
      as_codebook(.lookups = .lookups, .miss_propagate = .miss_propagate)
  }

  process_metadata <- function(.meta,
                              name = name,
                              label = label,
                              value_labels = value_labels,
                              ...,
                              type = NULL,
                              .sep1,
                              .sep2,
                              .sep1_out = " = ",
                              .sep2_out = "; ",
                              .rmv_html = !name,
                              .rmv_line_breaks = !name) {
    out <- .meta |>
      as_codebook() |>
      meta_select_fields(
        name = {{ name }},
        type = {{ type }},
        label = {{ label }},
        value_labels = {{ value_labels }},
        ...
      ) |>
      meta_clean_fields(
        rmv_html = {{ .rmv_html }}, rmv_line_breaks = {{ .rmv_line_breaks }}
      ) |>
      meta_parse_val_labels(
        sep1 = .sep1, sep2 = .sep2, sep1_out = .sep1_out, sep2_out = .sep2_out
      )
  }

  write_codebook <- function(x, 
                             file, 
                             dataset_name = NULL, 
                             # add optional dimensions to output
                             incl_date = TRUE, 
                             overwrite = TRUE) {
    if (!is.null(dataset_name)) {
      dataset_name <- stringr::str_c("Dataset: ", dataset_name)
    }
    cb_date <- if (incl_date) stringr::str_c("Generated ", Sys.Date()) else NULL
    info <- c(dataset_name, cb_date)
    cols <- seq_along(x)
    nrows <- nrow(x)
    dat_start <- length(info) + 1
    all_rows <- seq(dat_start + nrows)
    dat_rows <- tail(all_rows, nrows)
    banded_rows <- dat_rows[seq(1, nrows, by = 2)]
    style_header <- openxlsx::createStyle(
      textDecoration = "italic", border = c("top", "bottom"), 
      borderColour = c("#808080", "black"), borderStyle = c("thin", "double")
    )
    style_bold <- openxlsx::createStyle(textDecoration = "bold")
    style_white <- openxlsx::createStyle(fgFill = "white")
    style_gray <- openxlsx::createStyle(fgFill = "#EEEEEE")
    wb <- openxlsx::createWorkbook()
    cb <- "Codebook"
    openxlsx::addWorksheet(wb, cb)
    if (!is.null(info)) {
      openxlsx::writeData(wb, cb, info)
      openxlsx::addStyle(wb, cb, style_bold, rows = 1:2, cols = 1)
    }
    openxlsx::writeData(
      wb, cb, x, headerStyle = style_header, startRow = dat_start
    )
    openxlsx::addStyle(
      wb, cb, 
      style_white, rows = all_rows, cols = cols, gridExpand = TRUE, stack = TRUE
    )
    openxlsx::addStyle(
      wb, cb, style_gray, rows = banded_rows, cols = cols, gridExpand = TRUE
    )
    openxlsx::setColWidths(wb, cb, cols = cols, widths = "auto")
    openxlsx::freezePane(
      wb, cb, firstActiveRow = dat_start + 1, firstActiveCol = 2
    )

    ok <- openxlsx::saveWorkbook(
      wb, file, overwrite = overwrite, returnValue = TRUE
    )
    if (!ok) cli::cli_abort("Failed to save codebook")
    invisible(file)
  }
}


# from Jeff's notes:
### Some checkbox response options function as "None of the above;" if selected, 
### no other option can be selected. This is denoted in the field_annotation 
### column with the handle "@NONEOFTHEABOVE = 'val1, val2, val3'. I often use 
### this for the 'Decline' option. When a participant selects one of these 
### none-of-the-above options, I've been asked to set all the other responses 
### in that checkbox set to that value as well, so they aren't counted as 0 
### (the default unchecked value) during data summaries.
# 1 - I don't think there are any examples of "@NONEOFTHEABOVE" in the 
#     sample data
# 2 - it sounds like "@NONEOFTHEABOVE" doesn't really mean "none of the above"
#     -- i.e., "all of the above are false" -- but instead means "missing" or 
#     "declined to answer" -- i.e., unknown whether the above are true or false.
#     need to understand this better and maybe add logic for it.
# 3 - for now, I instead just implemented setting all columns to user-missing 
#     if the column for the user-missing column == 1.

user_missing_to_na <- function(dat, 
                               user_missing = NULL, 
                               vars = tidyselect::everything()) {
  if (is.null(user_missing)) return(dat)
  user_missing <- as.character(user_missing)
  dat |>
    dplyr::mutate(dplyr::across(
      {{ vars }},
      \(x) dplyr::if_else(as.character(x) %in% user_missing, NA, x)
    ))
}

generate_codebook <- function(data,
                              metadata,
                              add_type = !("type" %in% names(metadata))) {
  # dat_user_missings <- user_missing_to_na(dat, user_missing)
  out <- tibble::tibble(name = names(data)) |>
    dplyr::left_join(meta, dplyr::join_by(name)) |>
    dplyr::mutate(missing = missing_string(data), .after = value_labels)
  if (add_type) {
    out <- out |>
      dplyr::mutate(
        type = ifelse(!is.na(value_labels), "categorical", sapply(data, class_collapse)),
      .after = name
    )
  }
  out
}


# # to be passed as arg
user_missing <- -99
# # then as.character in Fx
user_missing <- as.character(user_missing)

meta <- process_metadata(
    ugh_codebook,
    name = field_name,
    rc_type = field_type,
    label = field_label,
    value_labels = select_choices_or_calculations,
    form = form_name,
    .sep1 = ", ",
    .sep2 = "\\|"
  ) |>
  meta_parse_checkboxes_rc(datanames = names(ugh), user_missing = user_missing, label_vals = "meta") |> 
    # meta_parse_checkboxes_rc(datanames = names(ugh), label_vals = "meta") |> 
  meta_parse_complete_rc()

cb <- generate_codebook(ugh, meta)
cb |> in_excel()

codebook_out <- codebook |>
  dplyr::relocate(type, .after = name) |>
  dplyr::select(!tidyselect::any_of(c("rc_type"))) |>
  dplyr::relocate(form, .after = name) |> 
  dplyr::rename_with(
    \(x) stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
  )

path <- file.path("..", "Tests", "out2.xlsx")
write_codebook(codebook_out, path, "UGH sus REDCap") |> 
  lighthouse::open_file()
# probably do type-checking later, and consider whether value labels exist





cb_start <- function(dat) {
  tibble::tibble(
    ID = seq_along(dat),
    name = names(dat),
    type = sapply(dat, \(x) paste(class(x), collapse = ", ")),
  )
}

prep_meta_redcap <- function(meta, keep = NULL, val_sep1 = ", ", val_sep2 = "\\|", replace_break = " / ") {
 |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), strip_html)
    )
}
ugh |> dplyr::count(p45_p57)
x2 <- prep_meta_redcap(ugh_codebook) |> dplyr::select(!label)

# # A tibble: 45 × 1
#    label
#    <chr>
#  1 "HIDDEN. Time frame for assessment"
#  2 "HIDDEN. Time frame for assessment"
#  3 "[p45_timeframe], have you received substance abuse treatment of any type? […
#  4 "[p45_timeframe], did you end any substance abuse treatment (not including F…
#  5 "Did you receive outpatient treatment?"
#  6 "How many visits?"
#  7 "Attention\n\nThe number you entered is greater than the number of days in t…
#  8 "Did you receive Day Treatment?"
#  9 "How many days?"
# 10 "Attention\n\nThe number you entered is greater than the number of days in t…
# # ℹ 35 more rows

# cb_labels_redcap <- function(cb, meta) {

# }

meta <- ugh_codebook
cb |>
  dplyr::left_join(meta, dplyr::join_by(name == field_name))

# label
# value labels
missing <- missing_string(dat)

cb |>
  dplyr::left_join(ugh_codebook, dplyr::join_by(name == field_name))

ugh_codebook |> lighthouse::in_excel()
  dplyr::count(field_type)
ugh

lighthouse::cols_info(ugh) |> dplyr::count(class, type)
