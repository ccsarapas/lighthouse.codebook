row_divisions <- function(wb, sheet_name, data, rows, cols, variable_bands, variable_borders, sub_borders, sub_borders_var) {
  index_repeats <- function(data, var) {
    data |>
      dplyr::mutate(idx = {{ var }} != dplyr::lag({{ var }}, default = "")) |>
      dplyr::pull(idx)
  }
  block_bottoms <- function(chg_rows, rows) {
    c(rows$dat[which(chg_rows)] - 1, max(rows$dat))
  }
    if (variable_bands || variable_borders || sub_borders) {
    chg_rows <- index_repeats(data, Name)
    if (variable_bands) {
      rows$bands <- rows$dat[
        dplyr::near(dplyr::consecutive_id(data$Name) %% 2, 1)
      ]
    }
    if (variable_borders) {
      rows$var_borders <- block_bottoms(chg_rows, rows)[-1]
      cols$var_borders <- cols$all
    }
    if (variable_bands || variable_borders) data$Name[!chg_rows] <- ""
    if (sub_borders) {
      sub_var_chr <- as.character(rlang::ensym(sub_borders_var))
      chg_rows_sub <- index_repeats(data, {{ sub_borders_var }}) & !chg_rows
      rows$sub_borders <- block_bottoms(chg_rows_sub, rows)
      cols$sub_borders <- seq(match(sub_var_chr, names(data)), max(cols$all))
      data <- data |>
        dplyr::mutate("{sub_var_chr}" := ifelse(
          chg_rows_sub | chg_rows, 
          {{ sub_borders_var }}, 
          ""
        ))
    }
  }
  if (variable_bands) {
  wb <- wb |>
    openxlsx2::wb_add_fill(
      sheet_name, openxlsx2::wb_dims(rows$bands, cols$all),
      openxlsx2::wb_color(hex = "EEEEEE")
    )
  }
  for (nm in intersect(names(cols), c("var_borders", "sub_borders"))) {
    colors <- list(var_borders = "404040", sub_borders = "808080")
    for (r in rows[[nm]]) {
      wb <- wb |>
        openxlsx2::wb_add_border(
          sheet_name, openxlsx2::wb_dims(r, cols[[nm]]),
          bottom_color = openxlsx2::wb_color(hex = colors[[nm]]),
          top_border = NULL, left_border = NULL, right_border = NULL
        )
    }
  }
  wb
}

cb_write_sheet <- function(wb,
                           data, 
                           sheet_name,
                           header = NULL,
                           cols_pct = NULL,
                           cols_int = NULL,
                           group_cols = NULL,
                           id_cols = NULL,
                           incl_group_col_names = TRUE,
                           variable_bands = TRUE,
                           variable_borders = FALSE,
                           sub_borders = NULL) {
  # init sheet
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet_name)  
  pct_nms <- untidyselect(data, {{cols_pct}})
  int_nms <- untidyselect(data, {{ cols_int }})
  group_var_nms <- NULL
  decked <- NULL
  if (!missing(group_cols)) {
    group_var_nms <- untidyselect(data, {{ group_cols }})
    id_var_nms <- untidyselect(data, {{ id_cols }})
    group_lvls <- lapply(
      setNames(nm = group_var_nms),
      \(x) as.character(sort(unique(data[[x]])))
    )
    stat_var_nms <- setdiff(names(data), c(id_var_nms, group_var_nms))
    cols_decked <- tidyr::expand_grid(!!!c(group_lvls, list(cols = stat_var_nms)))
    if (incl_group_col_names) {
      cols_decked <- cols_decked |>
        dplyr::mutate(
          dplyr::across(!cols, \(x) stringr::str_c(dplyr::cur_column(), " = ", x))
        )
    }
    cols_decked <- cols_decked |>
      dplyr::mutate(
        dplyr::across(!cols, dplyr::consecutive_id, .names = "{.col}_set")
      )
    sets_lowest <- cols_decked[[ncol(cols_decked)]]
    breaks_at <- which(diff(sets_lowest) == 1)
    cols_decked <- cols_decked |>
      add_empty_rows(.after = breaks_at) |>
      dplyr::mutate(
        # fill values only if prev and following match
        dplyr::across(!cols, \(x) {
          dplyr::coalesce(
            x,
            ifelse(dplyr::lag(x) == dplyr::lead(x), dplyr::lag(x), x)
          )
        })
      )

    cols_id_decked <- tibble::tibble(cols = id_var_nms) |>
      dplyr::bind_rows(cols_decked) |>
      dplyr::mutate(cols = ifelse(is.na(cols), "_empty_", cols))

    data <- data |>
      dplyr::mutate(..spacer = NA) |>
      tidyr::pivot_wider(
        id_cols = {{ id_cols }},
        names_from = {{ group_cols }},
        values_from = tidyselect::all_of(c("..spacer", stat_var_nms)),
        names_vary = "slowest"
      ) |>
      as.data.frame()
    first_spacer <- grep("^\\.\\.spacer", names(data))[[1]]
    data[, first_spacer] <- NULL

    names(data) <- cols_id_decked$cols

    decked <- cols_id_decked |>
      dplyr::select({{ group_cols }}) |>
      t() |>
      unname()
    # decked_sets <- cols_id_decked |>
    #   dplyr::select(tidyselect::ends_with("_set"))
  }
  cols <- list(
    all = seq_along(data),
    num = unname(which(sapply(data, is.numeric))),
    pct = which(names(data) %in% pct_nms),
    int = which(names(data) %in% int_nms)
  )
  nrows <- nrow(data)
  rows <- tibble::lst(
    dat_start = length(header) + length(group_var_nms) + 1,
    decked_start = length(header) + 1,
    decked = seq_along(group_var_nms) + decked_start,
    header = seq_along(header),
    dat = seq_len(nrows) + dat_start,
    all = seq_len(nrows + dat_start)
  )
  names(data) <- make.unique(names(data), sep = "_tmp_")
  
  num_fmts <- list(num = "0.00", pct = "0.0%", int = "0")
  
  ## prep data for writing
  data <- data |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), \(x) tidyr::replace_na(x, ""))
    )
  
  ## Apply styles
  wb <- wb |>
    openxlsx2::wb_add_font(
      sheet_name, openxlsx2::wb_dims(rows$all, cols$all), "Aptos Narrow"
    ) |>
    openxlsx2::wb_add_font(
      sheet_name, openxlsx2::wb_dims(rows$dat_start, cols$all),
      italic = TRUE
    ) |>
    openxlsx2::wb_add_fill(
      sheet_name, openxlsx2::wb_dims(rows$all, cols$all),
      openxlsx2::wb_color("white")
    ) |>
    openxlsx2::wb_add_border(
      sheet_name, openxlsx2::wb_dims(max(rows$header), cols$all),
      bottom_color = openxlsx2::wb_color(hex = "808080"), 
      top_border = NULL, left_border = NULL, right_border = NULL
    ) |> 
    openxlsx2::wb_add_border(
      sheet_name, openxlsx2::wb_dims(rows$dat_start, cols$all), 
      # bottom_color = openxlsx2::wb_color("black"), 
      bottom_border = "double",
      top_border = NULL, left_border = NULL, right_border = NULL
    )

  wb <- row_divisions(
    wb, sheet_name, data, rows, cols, variable_bands, variable_borders, 
    sub_borders = !missing(sub_borders), sub_borders_var = {{ sub_borders }}
  )

  if (length(cols$num)) {
    wb <- wb |>
      openxlsx2::wb_add_cell_style(
        sheet_name, 
        openxlsx2::wb_dims(c(rows$dat_start, rows$dat), cols$num),
        horizontal = "right"
      )
    for (nm in names(num_fmts)) {
      wb <- wb |>
        openxlsx2::wb_add_numfmt(
          sheet_name, openxlsx2::wb_dims(rows$dat, cols[[nm]]), 
          numfmt = num_fmts[[nm]]
        )
    }
  }
  
  names(data) <- gsub("_tmp_.+", "", names(data))
  names(data) <- gsub("_empty_", "", names(data))
  ## Write data
  wb <- wb |>
    openxlsx2::wb_add_data(
      sheet_name, data,
      start_row = rows$dat_start, start_col = 1,
      na.strings = "-"
    )
  if (!is.null(decked)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        sheet_name, decked, start_row = rows$decked_start, 
        start_col = 1, col_names = FALSE, na.strings = ""
      )
    for (i in seq_along(rows$decked)) {
      wb <- openxlsx2::wb_add_border(
        wb, sheet_name, openxlsx2::wb_dims(
          i, which(!is.na(decked[i, ])), from_row = rows$decked_start
        ),
        top_border = NULL, left_border = NULL, right_border = NULL
      )
    }
  }
  if (length(header)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        sheet_name, header, start_row = 1, start_col = 1, na.strings = ""
      ) |>
      openxlsx2::wb_add_font(
        sheet_name, openxlsx2::wb_dims(rows$header, 1), bold = TRUE
      )
  }
  # Set column widths and freeze panes
  wb <- wb |>
    openxlsx2::wb_set_col_widths(sheet_name, cols$all, widths = "auto") |>
    openxlsx2::wb_freeze_pane(
      sheet_name, first_active_row = rows$dat_start + 1,  first_active_col = 2
    )
  
  wb
}
  


