
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


wb_row_borders_by <- function(wb,
                              sheet = openxlsx2::current_sheet(),
                              dims,
                              by,
                              start_col = NULL,
                              data,
                              skip_first = TRUE,
                              color = openxlsx2::wb_color(hex = "404040"),
                              border = "thin",
                              update = FALSE,
                              ...) {
  index_repeats <- function(data, by) {
    data |>
      dplyr::mutate(
        idx = dplyr::if_any({{ by }}, \(x) x != dplyr::lag(x, default = ""))
      ) |>
      dplyr::pull(idx)
  }
  block_bottoms <- function(chg_rows, rows) {
    c(rows[which(chg_rows)] - 1, max(rows))
  }
  rowcol <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  chg_rows <- index_repeats(data, {{ by }})
  rows <- block_bottoms(chg_rows, rowcol$row)
  if (skip_first) rows <- rows[-1]
  cols <- seq(min(rowcol$col), max(rowcol$col))
  start_quo <- rlang::enquo(start_col)
  if (!rlang::quo_is_null(start_quo)) {
    idx <- seq(tidyselect::eval_select(start_quo, data), ncol(data))
    cols <- cols[idx]
  }
  for (r in rows) {
    wb <- wb |>
      openxlsx2::wb_add_border(
        sheet, openxlsx2::wb_dims(r, cols),
        bottom_color = color, bottom_border = border,
        top_border = NULL, left_border = NULL, right_border = NULL,
        update = update, ...
      )
  }
  wb
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
                           group_cols = NULL,
                           incl_group_col_names = TRUE) {
  # init sheet
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet_name)
  data_nms <- names(data)
  num_nms <- data_nms[sapply(data, is.numeric)]
  pct_nms <- untidyselect(data, {{cols_pct}})
  int_nms <- untidyselect(data, {{ cols_int }})
  group_var_nms <- NULL
  decked <- NULL
  if (!missing(group_cols)) {
    group_var_nms <- untidyselect(data, {{ group_cols }})
    val_cols <- rlang::quo(!c({{ group_cols }}, {{ id_cols }}))    
    data <- data |>
      dplyr::mutate(..spacer = NA_character_) |>
      # needed to ensure correct column order when >1 group var
      dplyr::arrange(dplyr::pick({{ group_cols }})) |> 
      tidyr::pivot_wider(
        id_cols = {{ id_cols }},
        names_from = tidyselect::all_of(rev(group_var_nms)),
        values_from = c(..spacer, {{ val_cols }}),
        names_sep = "_SEP_",
        names_vary = "slowest"
      )
    
    first_spacer <- grep("^\\.\\.spacer", names(data))[[1]]
    data[, first_spacer] <- NULL
    
    decked <- strsplit(names(data), "_SEP_")
    decked <- do.call(cbind, lighthouse::pad_vectors(!!!decked))

    data_nms <- decked[1, ]
    data_nms[data_nms == "..spacer"] <- ""
    
    decked <- decked[-1, ]
    decked <- decked[rev(seq(nrow(decked))), ]
    if (incl_group_col_names) {
      decked <- apply(decked, 2, \(x) stringr::str_c(group_var_nms, " = ", x))
    }
    decked_grp <- decked[, -1]
    decked_grp_lag <- decked[, -ncol(decked)]
    decked0 <- decked
    decked[, -1][decked_grp != decked_grp_lag] <- NA

    decked_fmt <- apply(decked, 1, \(r) {
      loc_valid <- which(!is.na(r))
      consec <- dplyr::consecutive_id(r[loc_valid])
      merge <- unname(split(loc_valid, consec))
      list(rule = loc_valid, merge = merge)
    })
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
    by <- rlang::expr(c(!!rows_border_by, !!rows_sub_border_by))
    wb <- wb |>
      wb_row_borders_by(
        dims = openxlsx2::wb_dims(rows$dat, cols$all), by = !!by, data = data,
        start_col = !!rows_sub_border_by, 
        color = openxlsx2::wb_color(hex = "808080"),
      )
  }
  if (!rlang::quo_is_null(rows_border_by)) {
    wb <- wb |>
      wb_row_borders_by(
        dims = openxlsx2::wb_dims(rows$dat, cols$all), by = !!rows_banded_by, 
        data = data
      )
  }
  clear_repeats <- untidyselect(data, {{ clear_repeats }})
  if (length(clear_repeats)) {
    for (i in seq(length(clear_repeats), 1)) {
      var <- clear_repeats[[i]]
      grp <- clear_repeats[seq(length.out = i - 1)]
      data <- data |>
        dplyr::mutate(
          "{var}" := repeats_to_blank(.data[[var]]),
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
  wb <- wb |>
    openxlsx2::wb_add_data(
      x = data, start_row = rows$dat_start, start_col = 1, na.strings = "-"
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


cb_write_codebook <- function(cb, 
                              file, 
                              dataset_name,
                              incl_date = TRUE,
                              incl_dims = TRUE,
                              detail_missing = TRUE,
                              group_by = NULL,
                              overwrite = TRUE
  ) {
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

  # prep sheet data
  overview <- cb_format_names(cb)
  summary_num <- cb |>
    cb_summarize_numeric() |> 
    cb_format_names(!c(`Valid n`, SD, MAD))
  summary_cat <- cb |>
    cb_summarize_categorical(detail_missing = detail_missing) |>
    cb_format_names(!n)
  
  # write sheets
  wb <- openxlsx2::wb_workbook() |>
    cb_write_sheet(
      overview, "Overview", header = h_overview, cols_pct = Missing
    ) |>
    cb_write_sheet(
      summary_num, "Summary - Numeric Vars",
      header = h_summ_num, cols_pct = `Valid %`, cols_int = `Valid n`
    )
  if (detail_missing) {
    wb <- wb |>
      cb_write_sheet(
        summary_cat, "Summary - Categorical", header = h_summ_cat,
        cols_pct = tidyselect::starts_with("%"), cols_int = n,
        rows_border_by = Name, rows_sub_border_by = `Valid / Missing`,
        clear_repeats = c(Name, Label, `Valid / Missing`)
      )
  } else {
    wb <- wb |>
      cb_write_sheet(
        summary_cat, "Summary - Categorical", header = h_summ_cat,
        cols_pct = tidyselect::starts_with("%"), cols_int = n,
        clear_repeats = c(Name, Label)
      )
  }
  
  # prep and write grouped sheets
  group_by_chr <- untidyselect(attr(cb, "data_zapped"), {{ group_by }})
  if (length(group_by_chr)) {
    h_summ_num_grp <- c(h_summ_num, paste("By ", toString(group_by_chr)))
    h_summ_cat_grp <- c(h_summ_cat, paste("By ", toString(group_by_chr)))
    summary_num_grp <- cb |>
      cb_summarize_numeric(group_cols = {{ group_cols }}) |>
      cb_format_names(!c(`Valid n`, SD, MAD, {{ group_cols }}))
    if (detail_missing) {
      cli::cli_inform(c(
        "i" = "Detailed missing value information is not currently supported for grouped summaries."
      ))
    }
    summary_cat_grp <- cb |>
      cb_summarize_categorical(
        group_cols = {{ group_cols }}, detail_missing = FALSE
      ) |>
      cb_format_names(!c(n, {{ group_cols }}))
    wb <- wb |>
      cb_write_sheet(
        summary_num_grp, "Grouped Summary - Numeric", header = h_summ_num_grp, 
        cols_pct = `Valid %`, cols_int = `Valid n`,
        id_cols = c(Name, Label), group_cols = {{ group_cols }}
      ) |>
      cb_write_sheet(
        summary_cat_grp, "Grouped Summary - Categorical", header = h_summ_cat_grp,
        cols_pct = tidyselect::starts_with("%"), cols_int = n,
        id_cols = c(Name, Label, Value), group_cols = {{ group_cols }},  
        clear_repeats = c(Name, Label)
      )
  }
  openxlsx2::wb_save(wb, file, overwrite = overwrite)
  invisible(file)
}