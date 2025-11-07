
cb_write_sheet <- function(wb,
                           data, 
                           sheet_name,
                           header = NULL,
                           format_names = TRUE,
                           cols_pct = NULL,
                           cols_int = NULL) {
  cols <- seq_along(data)
  nrows <- nrow(data)
  rows <- tibble::lst(
    dat_start = length(header) + 1,
    header = seq_along(header),
    dat = seq_len(nrows) + dat_start,
    all = seq_len(nrows + dat_start),
    banded = dat[seq(1, nrows, by = 2)]
  )
  num_fmts <- list(
    num = list(cols = which(sapply(data, is.numeric)), fmt = "0.00"),
    pct = list(cols = which(names(data) %in% untidyselect(data, {{cols_pct}})),
               fmt = "0.0%"),
    int = list(cols = which(names(data) %in% untidyselect(data, {{cols_int}})),
               fmt = "0")
  )
  
  # Add worksheet and set base styles
  wb <- openxlsx2::wb_add_worksheet(wb, sheet_name) |> 
    openxlsx2::wb_add_font(
      sheet_name, openxlsx2::wb_dims(rows$all, cols), "Aptos Narrow"
    ) |>
    openxlsx2::wb_add_fill(
      sheet_name, openxlsx2::wb_dims(rows$all, cols),
      openxlsx2::wb_color("white")
    )

  # Write header
  if (length(header)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        sheet_name, header, start_row = 1, start_col = 1, na.strings = ""
      ) |>
      openxlsx2::wb_add_font(
        sheet_name, openxlsx2::wb_dims(rows$header, 1), bold = TRUE
      )
  }
  
  # Format column names
  if (format_names) {
    names(data) <- stringr::str_to_title(
      stringr::str_replace_all(names(data), "_", " ")
    )
  }
  
  # Write data and apply styles
  wb <- wb |>
    openxlsx2::wb_add_data(
      sheet_name, data, start_row = rows$dat_start, start_col = 1, 
      na.strings = ""
    ) |>
    # Row banding
    openxlsx2::wb_add_fill(
      sheet_name, openxlsx2::wb_dims(rows$banded, cols),
      openxlsx2::wb_color(hex = "EEEEEE")
    ) |>
    # Style variable names
    openxlsx2::wb_add_font(
      sheet_name, openxlsx2::wb_dims(rows$dat_start, cols), italic = TRUE
    ) |>
    openxlsx2::wb_add_border(
      sheet_name, openxlsx2::wb_dims(rows$dat_start, cols), 
      top_color = openxlsx2::wb_color(hex = "808080"), top_border = "thin",
      bottom_color = openxlsx2::wb_color("black"), bottom_border = "double",
      left_border = "none", right_border = "none"
    )
  
  # Format and style numeric columns
  if (length(num_fmts$num$cols)) {
    wb <- wb |>
      openxlsx2::wb_add_cell_style(
        sheet_name, openxlsx2::wb_dims(rows$dat_start, num_fmts$num$cols),
        horizontal = "right"
      )
    for (fmt in num_fmts) {
      wb <- wb |>
        openxlsx2::wb_add_numfmt(
          sheet_name, openxlsx2::wb_dims(rows$dat, fmt$cols), numfmt = fmt$fmt
        )
    }
  }
  # Set column widths and freeze panes
  wb <- wb |>
    openxlsx2::wb_set_col_widths(sheet_name, cols, widths = "auto") |>
    openxlsx2::wb_freeze_pane(
      sheet_name, first_active_row = rows$dat_start + 1,  first_active_col = 2
    )
  
  wb
}

incl_date = TRUE 
incl_dims = TRUE
dataset_name = "UGH sus REDCap"
file = file.path("..", "Tests", "summary_num2.xlsx")
overwrite = TRUE

cb_name <- cb_dims <- cb_date <- NULL
if (!is.null(dataset_name)) cb_name <- glue_chr("Dataset: {dataset_name}")
if (incl_dims) {
  cb_dims <- cli::pluralize(
    "{attr(cb, 'n_obs')} record{?s} x {attr(cb, 'n_vars')} variable{?s}"
  )
}
cb_date <- if (incl_date) glue_chr("Codebook generated {Sys.Date()}")
h_dict <- c(dataset_name, cb_dims, cb_date)
h_summ <- c(dataset_name, "Numeric variables summary")

openxlsx2::wb_workbook() |> 
  cb_write_sheet(codebook_out, "Overview", header = h_dict, cols_pct = missing) |> 
  cb_write_sheet(
    summ_num, "Summary - Numeric Vars", header = h_summ, format_names = FALSE, 
    cols_pct = `valid %`, cols_int = `valid n`
  ) |> 
  openxlsx2::wb_save(file, overwrite = overwrite)
# invisible(file)

lighthouse::open_file(file)


