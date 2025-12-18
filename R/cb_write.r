#' Write codebook and data summaries to an Excel workbook
#' 
#' `cb_write()` writes an Excel workbook to disk with tabs including a codebook; 
#' summary statistics for numeric variables; frequencies for categorical variables; 
#' truncated frequencies for text variables; and optional grouped summaries for numeric
#' and categorical variables. For data summaires, variables with value labels, factors, 
#' and logical variables are treated as categorical, numeric and integer variables 
#' are treated as numeric, and (unlabeled) character variables are treated as text.
#' 
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param file Path to write to.
#' @param dataset_name Name of the dataset to display in workbook headers.
#' @param incl_date,incl_dims Should the date and/or dataset dimensions be included 
#'   in the Overview tab header?
#' @param group_by <[`tidy-select`][dplyr_tidy_select]> Column or columns to group
#'   by. If specified, additional numeric and categorical summary tabs will be included
#'   with decked heads for specified groups. 
#' @param detail_missing Include detailed missing value information on categorical 
#'   and text summary tabs?
#' @param n_text_vals On the text summary tab, how many unique non-missing values 
#'   should be included for each variable? If there are more than `n_text_vals` 
#'   + 1 unique values, the `n_text_vals` most common non-missing values will be included. 
#' @param overwrite Overwrite existing file?
#'
#' @return Invisibly returns the path to the written Excel file. See []`cb_create()`]
#'   and variants, [`cb_summarize_numeric()`], [`cb_summarize_categorical()`], and 
#'   [`cb_summarize_text()`] for details on the objects written to the file.
#' 
#' @export
cb_write <- function(cb, 
                     file, 
                     dataset_name = NULL,
                     incl_date = TRUE,
                     incl_dims = TRUE,
                     group_by = NULL,
                     detail_missing = TRUE,
                     n_text_vals = 5,
                     overwrite = TRUE) {
  check_codebook(cb)
  summaries <- list(
    num = cb_summarize_numeric(cb),
    cat = cb_summarize_categorical(cb, detail_missing = detail_missing),
    txt = cb_summarize_text(
      cb, 
      n_text_vals = n_text_vals, 
      detail_missing = detail_missing
    )
  )
  group_by <- rlang::enquo(group_by)
  if (!rlang::quo_is_null(group_by)) {
    if (detail_missing) {
      cli::cli_inform(c(
        "i" = paste0(
          "Detailed missing value information is not currently supported for ", 
          "grouped summaries, so will be included only for ungrouped summaries."
        )
      ))
    }
    summaries$grouped <- list(
      num = cb_summarize_numeric(cb, group_by = !!group_by),
      cat = cb_summarize_categorical(cb, group_by = !!group_by)
    )
  }
  cb_write_codebook(
    cb, summaries,
    file = file, dataset_name = dataset_name, incl_date = incl_date, 
    incl_dims = incl_dims, overwrite = overwrite
  )
}

cb_format_names <- function(cb,
                            cols = tidyselect::everything(),
                            replace = c("pct" = "%"),
                            keep_lower = c("n", "of")) {
  format_fx <- function(x, keep_lower) {
    # change to title case unless in `keep_lower` or has any non-lowercase characters
    ifelse(
        x %in% keep_lower | stringr::str_to_lower(x) != x,
        x,
        stringr::str_to_title(x)
      ) |>
      stringr::str_c(collapse = " ")
  }
  old <- untidyselect(cb, {{ cols }})
  new <- old |>
    stringr::str_replace_all(replace) |>
    stringr::str_split("_") |>
    vapply(format_fx, character(1), keep_lower = keep_lower)
  new <- setNames(old, new)
  dplyr::rename(cb, !!!new)
}

wb_add_data_multi_na <- function(wb,
                                 sheet = openxlsx2::current_sheet(),
                                 x,
                                 start_col = 1,
                                 start_row = 1,
                                 na.strings = openxlsx2::na_strings(),
                                 ...) {
  if (length(na.strings) == 1) {
    wb |>
      openxlsx2::wb_add_data(
        x = x, start_row = start_row, start_col = start_col,
        na.strings = na.strings, ...
      )
  } else {
    if (length(na.strings) != ncol(x)) {
      cli::cli_abort(
        "{.arg na.strings} must be length 1 or length {.code ncol(x)}."
      )
    }
    na.cols <- split(seq_along(na.strings), data.table::rleid(na.strings))
    na.vals <- rle(na.strings)$values
    for (i in seq_along(na.cols)) {
      cols <- na.cols[[i]]
      vals <- na.vals[[i]]
      start <- start_col + cols[[1]] - 1
      wb <- wb |>
        openxlsx2::wb_add_data(
          x = x[, cols, drop = FALSE], start_row = start_row, start_col = start,
          na.strings = vals, ...
        )
    }
    wb
  }
}
  
wb_banded_fill_by <- function(wb,
                              sheet = openxlsx2::current_sheet(),
                              dims,
                              by,
                              data,
                              color = openxlsx2::wb_color(hex = "EEEEEE"),
                              pattern = "solid",
                              gradient_fill = "",
                              every_nth_col = 1,
                              bg_color = NULL,
                              ...) {
  rowcol <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  bands <- data |>
    dplyr::mutate(bands = dplyr::consecutive_id(dplyr::pick({{ by }}))) |>
    with(dplyr::near(bands %% 2, 1))
  rows <- rowcol$row[bands]
  cols <- seq(min(rowcol$col), max(rowcol$col))
  openxlsx2::wb_add_fill(
    wb, sheet, dims = openxlsx2::wb_dims(rows, cols),
    color = color, pattern = pattern, gradient_fill = gradient_fill,
    every_nth_col = every_nth_col, bg_color = bg_color, ...
  )
}

wb_row_borders <- function(wb,
                           sheet = openxlsx2::current_sheet(),
                           dims,
                           start_col = NULL,
                           data,
                           skip_first = TRUE,
                           color = openxlsx2::wb_color("404040"),
                           border = "thin",
                           update = FALSE,
                           ...) {
  style_nm <- paste0(border, color)
  existing <- wb$styles_mgr$get_dxf_id(style_nm)
  if (is.null(existing) || is.na(existing)) {
    style <- openxlsx2::create_dxfs_style(
      border = TRUE, border_style = NULL, border_color = NULL,
      top_color = color,
      top_style = border
    )
    wb <- openxlsx2::wb_add_style(wb, style, style_nm)
  }
  rowcol <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  rows <- rowcol$row
  if (skip_first) rows <- rows[-1]
  cols <- seq(min(rowcol$col), max(rowcol$col))
  start_quo <- rlang::enquo(start_col)
  if (!rlang::quo_is_null(start_quo)) {
    idx <- seq(tidyselect::eval_select(start_quo, data), ncol(data))
    cols <- cols[idx]
  }
  rule <- glue_chr(
    '{openxlsx2::wb_dims(min(rows), min(cols), fix = "col")}<>""'
  )
  wb |>
    openxlsx2::wb_add_conditional_formatting(
      dims = openxlsx2::wb_dims(rows, cols),
      rule = rule,
      style = style_nm
  )
}

cb_prep_grouped_data <- function(data, group_by, id_cols) {
  group_var_nms <- untidyselect(data, {{ group_by }})
  val_cols <- rlang::quo(!c({{ group_by }}, {{ id_cols }}))
  data
  data <- data |>
    dplyr::mutate(..spacer = NA_character_) |>
    # needed to ensure correct column order when >1 group var
    dplyr::arrange(dplyr::pick({{ group_by }})) |>
    tidyr::pivot_wider(
      id_cols = {{ id_cols }},
      names_from = tidyselect::all_of(rev(group_var_nms)),
      values_from = c(..spacer, {{ val_cols }}),
      names_sep = "_SEP_",
      names_vary = "slowest"
    )
  first_spacer <- grep("^\\.\\.spacer", names(data))[[1]]
  data[, first_spacer] <- NULL
  list(data, group_var_nms)
}

cb_prep_decked_cols <- function(data, group_var_nms, incl_group_col_names) {
  decked <- strsplit(names(data), "_SEP_")
  decked <- do.call(cbind, lighthouse::pad_vectors(!!!decked))

  data_nms <- decked[1, ]
  data_nms[data_nms == "..spacer"] <- ""

  decked <- decked[-1, , drop = FALSE]
  decked <- decked[rev(seq(nrow(decked))), , drop = FALSE]
  if (incl_group_col_names) {
    decked <- apply(decked, 2, \(x) stringr::str_c(group_var_nms, " = ", x))
    # if decked has only one row, `apply` returns vector, so coerce back to matrix
    if (!is.matrix(decked)) decked <- matrix(decked, nrow = 1)
  }
  decked_grp <- decked[, -1]
  decked_grp_lag <- decked[, -ncol(decked)]
  decked[, -1][decked_grp != decked_grp_lag] <- NA

  decked_fmt <- apply(decked, 1, \(r) {
    loc_valid <- which(!is.na(r))
    consec <- dplyr::consecutive_id(r[loc_valid])
    merge <- unname(split(loc_valid, consec))
    list(rule = loc_valid, merge = merge)
  })

  list(decked, data_nms, decked_fmt)
}

cb_write_sheet <- function(wb,
                           data, 
                           sheet_name,
                           header = NULL,
                           cols_pct = NULL,
                           cols_int = NULL,
                           rows_banded_by = Name,
                           rows_border_by = NULL,
                           rows_sub_border_by = NULL, 
                           clear_repeats = NULL,
                           id_cols = NULL,
                           group_by = NULL,
                           incl_group_col_names = TRUE) {
  # init sheet
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet_name)
  data_nms <- names(data)
  num_nms <- data_nms[sapply(data, is.numeric)]
  pct_nms <- untidyselect(data, {{ cols_pct }})
  int_nms <- untidyselect(data, {{ cols_int }})
  group_var_nms <- decked <- NULL
  if (!missing(group_by)) {
    c(data, group_var_nms) %<-% cb_prep_grouped_data(
      data = data, group_by = {{ group_by }}, id_cols = {{ id_cols }}
    )
    c(decked, data_nms, decked_fmt) %<-% cb_prep_decked_cols(
      data = data, group_var_nms = group_var_nms, 
      incl_group_col_names = incl_group_col_names
    )
  }
  
  cols <- list(
    all = seq_along(data),
    num = which(data_nms %in% num_nms),
    pct = which(data_nms %in% pct_nms),
    int = which(data_nms %in% int_nms)
  )
  nrows <- nrow(data)
  rows <- tibble::lst(
    dat_start = length(header) + length(group_var_nms) + 1,
    decked_start = length(header) + 1,
    decked = seq_along(group_var_nms) + length(header),
    header = seq_along(header),
    dat = seq_len(nrows) + dat_start,
    all = seq_len(nrows + dat_start)
  )
  
  num_fmts <- list(num = "0.00", pct = "0.0%", int = "0")
  
  ## prep data for writing
  data <- data |>
    nan_to_na() |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), \(x) tidyr::replace_na(x, ""))
    )
  
  ## Apply styles
  wb <- wb |>
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(rows$all, cols$all), name = "Aptos Narrow"
    ) |>
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(rows$dat_start, cols$all), italic = TRUE
    ) |>
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(rows$all, cols$all),
      color = openxlsx2::wb_color("white")
    ) |>
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(max(rows$header), cols$all),
      bottom_color = openxlsx2::wb_color(hex = "808080"), 
      top_border = NULL, left_border = NULL, right_border = NULL
    ) |> 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(rows$dat_start, cols$all), 
      bottom_border = "double",
      top_border = NULL, left_border = NULL, right_border = NULL
    )
  rows_banded_by <- rlang::enquo(rows_banded_by)
  rows_border_by <- rlang::enquo(rows_border_by)
  rows_sub_border_by <- rlang::enquo(rows_sub_border_by)
  if (!rlang::quo_is_null(rows_banded_by)) {
    wb <- wb |>
      wb_banded_fill_by(
        dims = openxlsx2::wb_dims(rows$dat, cols$all), by = !!rows_banded_by, 
        data = data
      )
  }
  if (!rlang::quo_is_null(rows_sub_border_by)) {
    wb <- wb |>
      wb_row_borders(
        dims = openxlsx2::wb_dims(rows$dat, cols$all), data = data,
        start_col = !!rows_sub_border_by, 
        color = openxlsx2::wb_color(hex = "808080")
      )
  }
  if (!rlang::quo_is_null(rows_border_by)) {
    wb <- wb |>
      wb_row_borders(
        dims = openxlsx2::wb_dims(rows$dat, cols$all), data = data,
        start_col = !!rows_border_by
      )
  }
  clear_repeats <- untidyselect(data, {{ clear_repeats }})
  if (length(clear_repeats)) {
    for (i in seq(length(clear_repeats), 1)) {
      var <- clear_repeats[[i]]
      grp <- clear_repeats[seq(length.out = i - 1)]
      replace <- ifelse(is.character(data[[var]]), "empty", "NA")
      data <- data |>
        dplyr::mutate(
          "{var}" := repeats_to_blank(.data[[var]], replace = replace),
          .by = tidyselect::all_of(grp)
        )
    }
  }
  if (length(cols$num)) {
    wb <- wb |>
      openxlsx2::wb_add_cell_style(
        dims = openxlsx2::wb_dims(c(rows$dat_start, rows$dat), cols$num),
        horizontal = "right"
      )
      
    for (nm in names(num_fmts)) {
      cols_nm <- cols[[nm]]
      if (length(cols_nm)) {
        wb <- wb |>
          openxlsx2::wb_add_numfmt(
            dims = openxlsx2::wb_dims(rows$dat, cols_nm), 
            numfmt = num_fmts[[nm]]
          )
      }
    }
  }

  data <- as.data.frame(data)
  names(data) <- data_nms

  ## Write data

  # the following, along with `wb_add_data_multi_na()`, is a workaround to use 
  # different `na.strings` for `Unique n` column
  na.strings <- "-"
  if ("Unique n" %in% data_nms) {
    na.strings <- rep(na.strings, ncol(data))
    na.strings[data_nms == "Unique n"] <- ""
  }
  
  wb <- wb |>
    wb_add_data_multi_na(
      x = data, start_row = rows$dat_start, start_col = 1, 
      na.strings = na.strings
    )  
  
  if (!is.null(decked)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        x = decked, start_row = rows$decked_start, start_col = 1, 
        col_names = FALSE, na.strings = ""
      ) |> 
      openxlsx2::wb_add_cell_style(
        dims = openxlsx2::wb_dims(rows$decked, cols$all), horizontal = "center"
      ) |> 
      openxlsx2::wb_add_font(
        dims = openxlsx2::wb_dims(rows$decked, cols$all),
        bold = TRUE, italic = TRUE
      )      
    for (i in seq_along(decked_fmt)) {
      fmt <- decked_fmt[[i]]
      wb <- wb |> 
        openxlsx2::wb_add_border(
          dims = openxlsx2::wb_dims(i, fmt$rule, from_row = rows$decked_start),
          top_border = NULL, left_border = NULL, right_border = NULL
        )
      for (cells in fmt$merge) {
        wb <- wb |>
          openxlsx2::wb_merge_cells(
            dims = openxlsx2::wb_dims(i, cells, from_row = rows$decked_start)
          )
      }
    }
  }
  if (length(header)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        x = header, start_row = 1, start_col = 1, na.strings = ""
      ) |>
      openxlsx2::wb_add_font(
        dims = openxlsx2::wb_dims(rows$header, 1), bold = TRUE
      )
  }
  # Set column widths and freeze panes
  wb <- wb |>
    openxlsx2::wb_set_col_widths(cols = cols$all, widths = "auto") |>
    openxlsx2::wb_freeze_pane(
      first_active_row = rows$dat_start + 1,  first_active_col = 2
    )
    
  
  wb
}

cb_valid_miss_col <- function(summary) {
  summary |>
    dplyr::mutate(
      n_val_miss = ifelse(
        is_missing,
        sum(n[is_missing]) / sum(n),
        sum(n[!is_missing]) / sum(n)
      ),
      is_missing = glue_chr(
        "{ifelse(is_missing, 'Missing', 'Valid')} ",
        "({sprintf('%.1f%%', n_val_miss * 100)})"
      ),
      n_val_miss = NULL,
      .by = name
    ) |>
    dplyr::rename(`valid / missing` = is_missing)
}

cb_write_codebook <- function(cb, 
                              summaries,
                              file, 
                              dataset_name = NULL,
                              incl_date = TRUE,
                              incl_dims = TRUE,
                              overwrite = TRUE,
                              min_col_width = 7,
                              max_col_width = 70) {
  # create headers
  cb_name <- cb_dims <- cb_date <- NULL
  if (!is.null(dataset_name)) cb_name <- glue_chr("Dataset: {dataset_name}")
  if (incl_dims) {
    cb_dims <- cli::pluralize(
      "{attr(cb, 'n_obs')} record{?s} x {attr(cb, 'n_vars')} variable{?s}"
    )
  }
  cb_date <- if (incl_date) glue_chr("Codebook generated {Sys.Date()}")
  h_overview <- c(dataset_name, cb_dims, cb_date)
  h_summ_num <- c(dataset_name, "Numeric variables summary")
  h_summ_cat <- c(dataset_name, "Categorical variables summary")
  h_summ_txt <- c(dataset_name, "Text variables summary")
  
  # set max col width
  opts <- options(
    openxlsx2.minWidth = min_col_width,
    openxlsx2.maxWidth = max_col_width
  )
  on.exit(options(opts))
    
  # write overview and ungrouped numeric sheets
  overview <- cb_format_names(cb)
  summaries$num <- cb_format_names(summaries$num)
  wb <- openxlsx2::wb_workbook() |>
    cb_write_sheet(
      overview, "Overview", header = h_overview, cols_pct = Missing
    ) |>
    cb_write_sheet(
      summaries$num, "Summary - Numeric",
      header = h_summ_num, cols_pct = `Valid %`, cols_int = `Valid n`
    )
    
  # write ungrouped categorical sheet 
  detail_missing <- attr(summaries$cat, "detail_missing")
  if (detail_missing) summaries$cat <- cb_valid_miss_col(summaries$cat)
  summaries$cat <- cb_format_names(summaries$cat)
  rows_border_by <- rows_sub_border_by <- NULL
  if (detail_missing) {
    rows_border_by <- rlang::sym("Name")
    rows_sub_border_by <- rlang::sym("Valid / Missing")
  }
  wb <- wb |>
    cb_write_sheet(
      summaries$cat, 
      "Summary - Categorical", 
      header = h_summ_cat,
      cols_pct = tidyselect::starts_with("%"), 
      cols_int = n,
      rows_border_by = !!rows_border_by, 
      rows_sub_border_by = !!rows_sub_border_by,
      clear_repeats = tidyselect::any_of(
        c("Name", "Label Stem", "Label", "Valid / Missing")
      )
    )

  # write ungrouped text sheet 
  detail_missing <- attr(summaries$txt, "detail_missing")
  if (detail_missing) summaries$txt <- cb_valid_miss_col(summaries$txt)
  summaries$txt <- cb_format_names(summaries$txt)
  rows_border_by <- rows_sub_border_by <- NULL
  if (detail_missing) {
    rows_border_by <- rlang::sym("Name")
    rows_sub_border_by <- rlang::sym("Valid / Missing")
  }
  wb <- wb |>
    cb_write_sheet(
      summaries$txt, 
      "Summary - Text", 
      header = h_summ_txt,
      cols_pct = tidyselect::starts_with("%"), 
      cols_int = c(`Unique n`, n),
      rows_border_by = !!rows_border_by, 
      rows_sub_border_by = !!rows_sub_border_by,
      clear_repeats = tidyselect::any_of(
        c("Name", "Label Stem", "Label", "Valid / Missing", "Unique n")
      )
    )
  
  # write grouped sheets
  grouped <- summaries$grouped
  if (!is.null(grouped)) {
    group_by <- attr(grouped$num, "group_by")
    group_by_chr <- untidyselect(attr(cb, "data_zapped"), !!group_by)
    h_summ_num_grp <- c(h_summ_num, paste("By ", toString(group_by_chr)))
    h_summ_cat_grp <- c(h_summ_cat, paste("By ", toString(group_by_chr)))
    grouped$num <- cb_format_names(grouped$num, !(!!group_by))
    grouped$cat <- cb_format_names(grouped$cat, !(!!group_by))
    wb <- wb |>
      cb_write_sheet(
        grouped$num, "Grouped Summary - Numeric", header = h_summ_num_grp, 
        cols_pct = `Valid %`, cols_int = `Valid n`,
        id_cols = tidyselect::any_of(c("Name", "Label Stem", "Label")), 
        group_by = !!group_by
      ) |>
      cb_write_sheet(
        grouped$cat, "Grouped Summary - Categorical", header = h_summ_cat_grp,
        cols_pct = tidyselect::starts_with("%"), cols_int = n,
        id_cols = tidyselect::any_of(c("Name", "Label Stem", "Label", "Value")), 
        group_by = !!group_by,  
        clear_repeats = tidyselect::any_of(c("Name", "Label Stem", "Label"))
      )
  }
  openxlsx2::wb_save(wb, file, overwrite = overwrite)
  invisible(file)
}
