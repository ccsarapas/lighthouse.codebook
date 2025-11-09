
cb_write_sheet <- function(wb,
                           data, 
                           sheet_name,
                           header = NULL,
                           cols_pct = NULL,
                           cols_int = NULL,
                           variable_bands = TRUE,
                           variable_borders = FALSE,
                           sub_borders = NULL) {
  cols <- list(
    all = seq_along(data),
    num = which(sapply(data, is.numeric)),
    pct = which(names(data) %in% untidyselect(data, {{cols_pct}})),
    int = which(names(data) %in% untidyselect(data, {{cols_int}}))
  )
  nrows <- nrow(data)
  rows <- tibble::lst(
    dat_start = length(header) + 1,
    header = seq_along(header),
    dat = seq_len(nrows) + dat_start,
    all = seq_len(nrows + dat_start)
  )
  num_fmts <- list(num = "0.00", pct = "0.0%", int = "0")
  
  ## prep data for writing
  data <- data |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), \(x) tidyr::replace_na(x, ""))
    )
  index_repeats <- function(data, var) {
    data |>
      dplyr::mutate(idx = {{ var }} != dplyr::lag({{ var }}, default = "")) |>
      dplyr::pull(idx)
  }
  block_bottoms <- function(chg_rows, rows) {
    c(rows$dat[which(chg_rows)] - 1, max(rows$dat))
  }
  if (variable_bands || variable_borders || !missing(sub_borders)) {
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
    if (!missing(sub_borders)) {
      sub_var_chr <- as.character(rlang::ensym(sub_borders))
      chg_rows_sub <- index_repeats(data, {{ sub_borders }}) & !chg_rows
      rows$sub_borders <- block_bottoms(chg_rows_sub, rows)
      cols$sub_borders <- seq(match(sub_var_chr, names(data)), max(cols$all))
      data <- data |>
        dplyr::mutate("{sub_var_chr}" := ifelse(
          chg_rows_sub | chg_rows, 
          {{ sub_borders }}, 
          ""
        ))
    }
  }
  
  ## Add worksheet and apply styles
  wb <- wb |>
    openxlsx2::wb_add_worksheet(sheet_name) |>
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
      sheet_name, openxlsx2::wb_dims(rows$dat_start, cols$all), 
      top_color = openxlsx2::wb_color(hex = "808080"), 
      bottom_color = openxlsx2::wb_color("black"), bottom_border = "double",
      left_border = NULL, right_border = NULL
    )
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

  ## Write data
  wb <- wb |>
    openxlsx2::wb_add_data(
      sheet_name, data,
      start_row = rows$dat_start, start_col = 1,
      na.strings = "-"
    )
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
  

