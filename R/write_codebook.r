
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
  block_bottoms <- function(var_change, rows) {
    c(rows$dat[which(var_change)][-1] - 1, max(rows$dat))
  }
  if (variable_bands || variable_borders || !missing(sub_borders)) {
    var_change <- index_repeats(data, Name)
    if (variable_bands) {
      rows$bands <- rows$dat[
        dplyr::near(dplyr::consecutive_id(data$Name) %% 2, 1)
      ]
    }
    if (variable_borders) {
      rows$var_borders <- block_bottoms(var_change, rows)
      cols$var_borders <- cols$all
    }
    if (variable_bands || variable_borders) data$Name[!var_change] <- ""
    if (!missing(sub_borders)) {
      sub_var_chr <- as.character(rlang::ensym(sub_borders))
      var_change_sub <- index_repeats(data, {{ sub_borders }})
      rows$sub_borders <- block_bottoms(var_change_sub, rows)
      cols$sub_borders <- seq(match(sub_var_chr, names(data)), max(cols$all))
      data <- data |>
        dplyr::mutate("{sub_var_chr}" := ifelse(
          var_change_sub | var_change, 
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
    for (r in rows[[nm]]) {
      wb <- wb |>
        openxlsx2::wb_add_border(
          sheet_name, openxlsx2::wb_dims(r, cols[[nm]]),
          bottom_color = openxlsx2::wb_color("black"),
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
  



# incl_date = TRUE 
# incl_dims = TRUE
# dataset_name = "UGH sus REDCap"
# file = file.path("..", "Tests", "summary.xlsx")
# overwrite = TRUE

# cb_name <- cb_dims <- cb_date <- NULL
# if (!is.null(dataset_name)) cb_name <- glue_chr("Dataset: {dataset_name}")
# if (incl_dims) {
#   cb_dims <- cli::pluralize(
#     "{attr(cb, 'n_obs')} record{?s} x {attr(cb, 'n_vars')} variable{?s}"
#   )
# }
# cb_date <- if (incl_date) glue_chr("Codebook generated {Sys.Date()}")
# h_dict <- c(dataset_name, cb_dims, cb_date)
# h_summ_num <- c(dataset_name, "Numeric variables summary")
# h_summ_cat <- c(dataset_name, "Categorical variables summary")


# codebook_out <- cb |>
#   dplyr::relocate(form, type, .after = name) |>
#   dplyr::select(!tidyselect::any_of(c("rc_type"))) |>
#   cb_format_names()
# summ_num <- cb_summarize_numeric(cb) |>
#   cb_format_names(!c(`Valid n`, SD, MAD))
# summ_cat <- cb_summarize_categorical(cb) |>
#   cb_format_names(!n)


# openxlsx2::wb_workbook() |>
#   cb_write_sheet(codebook_out, "Overview", header = h_dict, cols_pct = Missing) |>
#   cb_write_sheet(
#     summ_num, "Summary - Numeric Vars",
#     header = h_summ_num, cols_pct = `Valid %`, cols_int = `Valid n`
#   ) |>
#   cb_write_sheet(
#     summ_cat, "Summary - Categorical Vars",
#     header = h_summ_cat, cols_pct = tidyselect::starts_with("%"), cols_int = n,
#     variable_borders = TRUE, sub_borders = `Valid / Missing`
#   ) |> 
#   openxlsx2::wb_save(file, overwrite = overwrite)
# # invisible(file)

# lighthouse::open_file(file)
