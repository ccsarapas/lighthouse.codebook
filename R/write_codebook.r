
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
cb_add_dims <- function(cb) {
  data <- attr(cb, "data")
  set_attrs(cb, n_obs = nrow(data), n_vars = ncol(data))
}
write_codebook <- function(cb, 
                            file, 
                            dataset_name = NULL, 
                            incl_date = TRUE, 
                            incl_dims = TRUE,
                            format_names = TRUE,
                            overwrite = TRUE) {
  info <- write_codebook_header(
    dataset_name = dataset_name, incl_date = incl_date, incl_dims = incl_dims,
    nrows = attr(cb, "n_obs"), ncols = attr(cb, "n_vars")
  )
  nrows <- nrow(cb)
  cols <- list(all = seq_along(cb), missing = which(names(cb) == "missing"))
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
  openxlsx::addWorksheet(wb, "Codebook")
  if (!is.null(info)) {
    openxlsx::writeData(wb, "Codebook", info)
    openxlsx::addStyle(wb, "Codebook", styles$bold, rows = rows$info, cols = 1)
  }
  if (format_names) {
    names(cb) <- stringr::str_to_title(
      stringr::str_replace_all(names(cb), "_", " ")
    )
  }
  openxlsx::writeData(
    wb, "Codebook", cb, headerStyle = styles$header, startRow = rows$dat_start
  )
  openxlsx::addStyle(
    wb, "Codebook", styles$all, rows = rows$all, cols = cols$all, gridExpand = TRUE, 
    stack = TRUE
  )
  openxlsx::addStyle(
    wb, "Codebook", styles$gray, rows = rows$banded, cols = cols$all, 
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, "Codebook", styles$pct, rows = rows$dat, cols = cols$missing, 
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::setColWidths(wb, "Codebook", cols = cols$all, widths = "auto")
  openxlsx::freezePane(
    wb, "Codebook", firstActiveRow = rows$dat_start + 1, firstActiveCol = 2
  )

  ok <- openxlsx::saveWorkbook(
    wb, file, overwrite = overwrite, returnValue = TRUE
  )
  if (!ok) cli::cli_abort("Failed to save codebook")
  invisible(file)
}
