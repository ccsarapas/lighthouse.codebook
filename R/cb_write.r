#' Write codebook and data summaries to an Excel workbook
#' 
#' @description
#' `cb_write()` writes an Excel workbook to disk with tabs including a codebook; 
#' summary statistics for numeric variables; frequencies for categorical variables; 
#' truncated frequencies for text variables; and optional grouped summaries for numeric
#' and categorical variables. 
#' 
#' For data summaries, variables with value labels, factors, and logical variables 
#' are treated as categorical; numeric and integer variables are treated as numeric; 
#' and (unlabeled) character variables are treated as text. Summary tabs will be 
#' omitted if there are no variables of the relevant type.
#' 
#' @param cb An object of class `"li_codebook"` as produced by [`cb_create()`] or
#'   a variant.
#' @param file Path to write to.
#' @param dataset_name Name of the dataset to display in workbook headers.
#' @param group_by <[`tidy-select`][dplyr_tidy_select]> Column or columns to group
#'   by. If specified, additional numeric and categorical summary tabs will be included
#'   with grouped summaries. Subgroups are shown in columns by default. Some or all 
#'   grouping variables can instead be shown in rows if specified in `group_rows`, 
#' `group_rows_numeric`, or `group_rows_categorical`.
#' @param group_rows <[`tidy-select`][dplyr_tidy_select]> Column or columns to group 
#'   by in rows on grouped summary tabs. All columns must also be specified in `group_by`. 
#'   Will apply to both numeric and categorical summary tabs unless otherwise specified 
#'   in `group_rows_numeric` or `group_rows_categorical`.
#' @param group_rows_numeric,group_rows_categorical <[`tidy-select`][dplyr_tidy_select]> 
#'   Column or columns to group by in rows on grouped numeric or categorical summary 
#'   tab.
#' @param detail_missing Include detailed missing value information on ungrouped 
#'   categorical and text summary tabs? (Detailed missing information for grouped 
#'   summary tabs is not currently supported.)
#' @param n_text_vals On the text summary tab, how many unique non-missing values 
#'   should be included for each variable? If there are more than `n_text_vals` + 1 
#'   unique values, the `n_text_vals` most common non-missing values will be included. 
#' @param incl_date,incl_dims Should the date and/or dataset dimensions be included 
#'   in the Overview tab header?
#' @param hyperlinks If `TRUE`, variable names on the Overview sheet will link 
#'   to corresponding rows on summary tabs and vice versa.
#' @param overwrite Overwrite existing file?
#'
#' @return 
#' Invisibly returns the path to the written Excel file. The Excel workbook itself
#' will contain an "Overview" tab (see [`cb_create()`] and variants for details); 
#' "Summary - Numeric", "Summary - Categorical", and "Summary - Text" tabs if the
#' dataset includes variables of the relevant type (see [`cb_summarize_numeric()`],
#' [`cb_summarize_categorical()`], and [`cb_summarize_text()`]); and "Grouped Summary - Numeric" 
#' and "Grouped Summary - Categorical" tabs if `group_by` is specified. 
#' 
#' @section Alternative labelling for user missing values:
#' 
#' Different terminology for user missing values can be used by setting 
#' `options(lighthouse.codebook.nonresponse = TRUE)`. If set, the "User Missings"
#' column on the Overview sheet is instead called "Nonresponse Codes," and "Missing" 
#' values are instead referred to as "Excluded" values on overview and summary tabs.
#' 
#' @export
cb_write <- function(cb, 
                     file, 
                     dataset_name = NULL,
                     group_by = NULL,
                     group_rows = NULL,
                     group_rows_numeric = group_rows,
                     group_rows_categorical = group_rows,
                     detail_missing = c("if_any_user_missing", "yes", "no"),
                     n_text_vals = 5,
                     incl_date = TRUE,
                     incl_dims = TRUE,
                     hyperlinks = TRUE,
                     overwrite = TRUE) {
  check_codebook(cb)
  detail_missing <- match.arg(detail_missing)
  group_by <- cb_untidyselect(cb, {{ group_by }})
  group_rows <- cb_untidyselect(cb, {{ group_rows }})
  if (missing(group_rows_numeric)) {
    group_rows_numeric <- group_rows
  } else {
    group_rows_numeric <- cb_untidyselect(cb, {{ group_rows_numeric }})
  }
  if (missing(group_rows_categorical)) {
    group_rows_categorical <- group_rows
  } else {
    group_rows_categorical <- cb_untidyselect(cb, {{ group_rows_categorical }})
  }
  check_group_rows_arg(group_rows, group_by)
  check_group_rows_arg(group_rows_numeric, group_by)
  check_group_rows_arg(group_rows_categorical, group_by)
  cb_write_impl(
    cb = cb, 
    file = file, 
    dataset_name = dataset_name,
    group_by = group_by,
    group_rows = group_rows,
    group_rows_numeric = group_rows_numeric,
    group_rows_categorical = group_rows_categorical,
    detail_missing = detail_missing,
    n_text_vals = n_text_vals,
    incl_date = incl_date,
    incl_dims = incl_dims,
    hyperlinks = hyperlinks,
    overwrite = overwrite
  )
}

cb_write_impl <- function(cb, 
                          file, 
                          dataset_name = NULL,
                          group_by = NULL,
                          group_rows = NULL,
                          group_rows_numeric = group_rows,
                          group_rows_categorical = group_rows,
                          detail_missing = c("if_any_user_missing", "yes", "no"),
                          n_text_vals = 5,
                          incl_date = TRUE,
                          incl_dims = TRUE,
                          hyperlinks = TRUE,
                          overwrite = TRUE) {
  detail_missing <- detail_missing == "yes" || (
    detail_missing == "if_any_user_missing" && length(attr(cb, "user_missing"))
  )
  summaries <- list(
    num = cb_summarize_numeric_impl(cb),
    cat = cb_summarize_categorical_impl(cb, detail_missing = detail_missing),
    txt = cb_summarize_text_impl(
      cb,
      n_text_vals = n_text_vals,
      detail_missing = detail_missing
    )
  )
  if (!is.null(group_by)) {
    summaries$num_grp <- cb_summarize_numeric_impl(
      cb, 
      group_by = group_by, 
      group_rows = group_rows_numeric
    )
    summaries$cat_grp <- cb_summarize_categorical_impl(
      cb,
      group_by = group_by,
      group_rows = group_rows_categorical
    )
  }
  cb_write_codebook(
    cb, summaries,
    file = file, dataset_name = dataset_name, incl_date = incl_date, 
    incl_dims = incl_dims, hyperlinks = hyperlinks, overwrite = overwrite
  )
}

format_names <- function(x,
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
  x |>
    stringr::str_replace_all(replace) |>
    stringr::str_split("_") |>
    vapply(format_fx, character(1), keep_lower = keep_lower)
}

cb_format_names <- function(data,
                            replace = c("pct" = "%"),
                            keep_lower = c("n", "of"),
                            skip = NULL,
                            attrs = NULL) {
  old <- setdiff(names(data), skip)
  new <- setNames(old, format_names(old, replace = replace, keep_lower = keep_lower))
  out <- dplyr::rename(data, !!!new)
  for (a in attrs) {
    attr(out, a) <- attr(data, a) |>
      setdiff(skip) |>
      format_names(replace = replace, keep_lower = keep_lower)
  }
  out
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

compute_banded_rows <- function(rows, by, data) {
  if (is.null(by)) return(NULL)
  bands <- data |>
    dplyr::mutate(bands = dplyr::consecutive_id(dplyr::pick(all_of(by)))) |>
    with(dplyr::near(bands %% 2, 1))
  rows[bands]
}

compute_border_cols <- function(data, cols, start_col = NULL) {
  if (is.null(start_col)) return(NULL)
  idx <- seq(match(start_col, names(data)), ncol(data))
  cols <- cols[idx]
}

wb_cond_row_borders <- function(wb,
                                sheet = openxlsx2::current_sheet(),
                                dims,
                                color = openxlsx2::wb_color("404040"),
                                border = "thin") {
  style_nm <- paste0(border, color)
  existing <- lighthouse::suppress_warnings_if(
    wb$styles_mgr$get_dxf_id(style_nm),
    "Could not find style(s)"
  )
  if (is.null(existing) || is.na(existing)) {
    style <- openxlsx2::create_dxfs_style(
      border = TRUE, border_style = NULL, border_color = NULL,
      top_color = color,
      top_style = border
    )
    wb <- openxlsx2::wb_add_style(wb, style, style_nm)
  }
  c(cols, rows) %<-% openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  rule <- glue_chr(
    '{openxlsx2::wb_dims(min(rows), min(cols), fix = "col")}<>""'
  )
  wb |>
    openxlsx2::wb_add_conditional_formatting(
      sheet = sheet, dims = dims, rule = rule, style = style_nm
  )
}

cb_prep_grouped_data <- function(data, group_rows, group_cols, id_cols) {
  if (is.null(group_cols)) {
    return(dplyr::relocate(data, all_of(c(id_cols, group_rows))))
  }
  attrs_keep <- c(
    "detail_missing", "group_by", "group_counts", "group_rows", "group_cols", 
    "id_cols"
  )
  attrs <- attributes(data)
  attrs <- attrs[intersect(names(attrs), attrs_keep)]
  value_cols <- setdiff(names(data), c(group_rows, group_cols, id_cols))

  data <- data |>
    dplyr::mutate(..spacer = NA_character_) |>
    # needed to ensure correct column order when >1 group var
    dplyr::arrange(forcats::fct_inorder(Name), dplyr::pick(all_of(group_cols))) |>
    tidyr::pivot_wider(
      id_cols = all_of(c(id_cols, group_rows)),
      names_from = all_of(rev(group_cols)),
      values_from = c(..spacer, all_of(value_cols)),
      names_sep = "_SEP_",
      names_vary = "slowest"
    ) |>
    set_attrs(!!!attrs)
  first_spacer <- grep("^\\.\\.spacer", names(data))[[1]]
  data[, first_spacer] <- NULL
  data
}

cb_prep_decked_cols <- function(data, 
                                group_cols, 
                                incl_group_col_n, 
                                incl_group_col_names) {
  if (is.null(group_cols)) {
    return(list(data_nms = names(data), decked = NULL, decked_fmt = NULL))
  }

  decked <- strsplit(names(data), "_SEP_")
  decked <- do.call(cbind, lighthouse::pad_vectors(!!!decked))
  
  data_nms <- decked[1, ]
  data_nms[data_nms == "..spacer"] <- ""

  decked <- decked[-1, , drop = FALSE]
  decked <- decked[rev(seq(nrow(decked))), , drop = FALSE]
  
  if (incl_group_col_n) {
    decked_in <- decked
    group_counts <- attr(data, "group_counts")
    for (i in seq(nrow(decked_in))) {
      rows_i <- decked_in[seq(i), , drop = FALSE]
      for (j in seq(ncol(decked_in))) {
        cols_j <- rows_i[, j]
        if (anyNA(cols_j)) next
        n <- sum(unlist(group_counts[[cols_j]]))
        decked[i, j] <- glue_chr("{decked[i, j]}\n(n = {n})")
      }
    }
  }

  if (incl_group_col_names) {
    decked <- apply(decked, 2, \(x) stringr::str_c(group_cols, " = ", x))
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

  list(data_nms = data_nms, decked = decked, decked_fmt = decked_fmt)
}

label_nonresponse <- function(params) {
  names(params$overview$data) <- stringr::str_replace_all(
    names(params$overview$data),
    c("User Missings" = "Nonresponse Codes", "Missing" = "Excluded")
  )
  for (sheet in c("cat", "txt", "cat_grp", "txt_grp")) {
    if (is.null(params[[sheet]])) next
    if (attr(params[[sheet]]$data, "detail_missing")) {
      names(params[[sheet]]$data) <- stringr::str_replace(
        names(params[[sheet]]$data), "Missing", "Excluded"
      )
      params[[sheet]]$data$`Valid / Excluded` <- stringr::str_replace(
        params[[sheet]]$data$`Valid / Excluded`, "Missing", "Excluded"
      )
    } else {
      idx_miss <- params[[sheet]]$data$Value == "(Missing)"
      params[[sheet]]$data$Value[idx_miss] <- "(Excluded)"
    }
  }
  params
}

var_name_hyperlinks <- function(params) {
  hl_rows <- intersect(names(params), c("overview", "num", "cat", "txt")) |>
    setNames(nm = _) |>
    lapply(\(nm) {
      name <- params[[nm]]$data$Name
      tibble::tibble(
        name = name[name != ""],
        "{paste0(nm, '_row')}" := params[[nm]]$rows$var_name
      )
    })

  get_sheet_rows <- function(sheet, hl_rows) {
    sheet <- sheet[[1]]
    if (is.na(sheet)) NA else tibble::deframe(hl_rows[[sheet]])
  }

  hl <- hl_rows$overview |>
    dplyr::mutate(
      overview_nm = params$overview$sheet_name,
      sheet = dplyr::case_match(
        name,
        hl_rows$num$name %||% NA ~ "num",
        hl_rows$cat$name %||% NA ~ "cat",
        hl_rows$txt$name %||% NA ~ "txt",
        .default = NA
      ),
    ) |>
    dplyr::mutate(
      sheet_nm = params[[sheet[[1]]]]$sheet_name %||% NA,
      sheet_row = get_sheet_rows(sheet, hl_rows)[name],
      .by = sheet
    ) |>
    dplyr::mutate(
      from_ov = ifelse(
        is.na(sheet_nm),
        NA,
        openxlsx2::create_hyperlink(
          sheet = sheet_nm, row = sheet_row, col = 1, text = name
        )
      ),
      to_ov = openxlsx2::create_hyperlink(
        sheet = overview_nm, row = overview_row, col = 1, text = name
      )
    )

  hl <- hl |>
    dplyr::transmute(sheet = "overview", row = overview_row, hl = from_ov) |>
    dplyr::bind_rows(dplyr::select(hl, sheet, row = sheet_row, hl = to_ov)) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      chunk = cumsum(tidyr::replace_na(row != dplyr::lag(row) + 1, FALSE)),
      .by = sheet
    )

  hl <- hl |>
    split(hl$sheet) |>
    lapply(\(x) split(x, x$chunk))
  
  for (nm in names(hl)) {
    params[[nm]]$hyperlinks <- hl[[nm]]
  }
  params
}

cb_prep_sheet_data <- function(data,
                               sheet_name = NULL, 
                               header = NULL,
                               cols_pct = NULL,
                               cols_int = NULL,
                               clear_repeats = NULL,
                               incl_group_col_n = TRUE,
                               incl_group_col_names = TRUE,
                               rows_banded_by = "Name",
                               rows_border_by = NULL,
                               rows_sub_border_by = NULL) {
  data_nms <- names(data)
  cols_num <- data_nms[sapply(data, is.numeric)]
  decked <- decked_fmt <- NULL
  group_by <- attr(data, "group_by")
  group_cols <- attr(data, "group_cols")
  group_rows <- attr(data, "group_rows")
  id_cols <- attr(data, "id_cols")
  if (!is.null(group_by)) {
    data <- cb_prep_grouped_data(
      data = data,
      group_rows = group_rows,
      group_cols = group_cols,
      id_cols = id_cols
    )
    c(data_nms, decked, decked_fmt) %<-% cb_prep_decked_cols(
      data = data, group_cols = group_cols,
      incl_group_col_n = incl_group_col_n,
      incl_group_col_names = incl_group_col_names
    )
    if (length(group_rows) > 1) {
      rows_border_by <- union("Name", rows_border_by)
      rows_sub_border_by <- union(head(group_rows, -1), rows_sub_border_by)
    }
  }

  cols <- tibble::lst(
    all = seq_along(data),
    num = which(data_nms %in% cols_num),
    pct = which(data_nms %in% cols_pct),
    int = which(data_nms %in% cols_int),
    border = lapply(
      rows_border_by, 
      \(rbb) compute_border_cols(data, cols = all, start_col = rbb)
    ),
    sub_border = lapply(
      rows_sub_border_by, 
      \(rsbb) compute_border_cols(data, cols = all, start_col = rsbb)
    )
  )
  
  nrows <- nrow(data)
  rows <- tibble::lst(
    dat_start = length(header) + length(group_cols) + 1,
    decked_start = length(header) + 1,
    decked = seq_along(group_cols) + length(header),
    header = seq_along(header),
    dat = seq_len(nrows) + dat_start,
    banded = compute_banded_rows(data, rows = dat, by = rows_banded_by),
    border = if (is.null(cols$border)) NULL else dat[-1],
    sub_border = border,
    all = seq_len(nrows + dat_start)
  )
  
  num_fmts <- list(num = "0.00", pct = "0.0%", int = "0")

  data <- data |>
    nan_to_na() |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.character), 
      \(x) tidyr::replace_na(x, "")
    ))
  if (length(clear_repeats)) {
    for (i in seq(length(clear_repeats), 1)) {
      var <- clear_repeats[[i]]
      grp <- clear_repeats[seq(length.out = i - 1)]
      if (is.character(data[[var]]) || is.factor(data[[var]])) {
        replace <- "empty"
      } else {
        replace <- "NA"
      }
      data <- data |>
        dplyr::mutate(
          "{var}" := repeats_to_blank(.data[[var]], replace = replace),
          .by = all_of(grp)
        )
    }
  }
  
  rows$var_name <- rows$dat[data$Name != ""]

  data <- as.data.frame(data)
  names(data) <- data_nms
  
  list(
    data = data,
    sheet_name = sheet_name,
    header = header,
    cols = cols,
    rows = rows,
    num_fmts = num_fmts,
    decked = decked,
    decked_fmt = decked_fmt
  )
}

cb_write_sheet <- function(wb, data, params) {
  pm <- params

  # init sheet
  wb <- wb |>
    openxlsx2::wb_add_worksheet(pm$sheet_name)
  
  ## apply styles
  wb <- wb |>
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(pm$rows$all, pm$cols$all), name = "Aptos Narrow"
    ) |>
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(pm$rows$dat_start, pm$cols$all), italic = TRUE
    ) |>
    openxlsx2::wb_add_fill(
      dims = openxlsx2::wb_dims(pm$rows$all, pm$cols$all),
      color = openxlsx2::wb_color("white")
    ) |>
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(max(pm$rows$header), pm$cols$all),
      bottom_color = openxlsx2::wb_color(hex = "808080"), 
      top_border = NULL, left_border = NULL, right_border = NULL
    ) |> 
    openxlsx2::wb_add_border(
      dims = openxlsx2::wb_dims(pm$rows$dat_start, pm$cols$all), 
      bottom_border = "double",
      top_border = NULL, left_border = NULL, right_border = NULL
    )

  if (!is.null(pm$rows$banded)) {
    wb <- wb |>
      openxlsx2::wb_add_fill(
        dims = openxlsx2::wb_dims(pm$rows$banded, pm$cols$all),
        color = openxlsx2::wb_color(hex = "EEEEEE")
      )
  }
  for (sb_cols in pm$cols$sub_border) {
    wb <- wb |>
      wb_cond_row_borders(
        dims = openxlsx2::wb_dims(pm$rows$sub_border, sb_cols),
        color = openxlsx2::wb_color(hex = "808080")
      )
  }
  for (b_cols in pm$cols$border) {
    wb <- wb |>
      wb_cond_row_borders(dims = openxlsx2::wb_dims(pm$rows$border, b_cols))
  }
  
  if (length(pm$cols$num)) {
    wb <- wb |>
      openxlsx2::wb_add_cell_style(
        dims = openxlsx2::wb_dims(c(pm$rows$dat_start, pm$rows$dat), pm$cols$num),
        horizontal = "right"
      )
      
    for (nm in names(pm$num_fmts)) {
      cols_nm <- pm$cols[[nm]]
      if (length(cols_nm)) {
        wb <- wb |>
          openxlsx2::wb_add_numfmt(
            dims = openxlsx2::wb_dims(pm$rows$dat, cols_nm), 
            numfmt = pm$num_fmts[[nm]]
          )
      }
    }
  }

  ## Write data

  # the following, along with `wb_add_data_multi_na()`, is a workaround to use 
  # different `na.strings` for `Unique n` column
  na.strings <- "-"
  if ("Unique n" %in% names(data)) {
    na.strings <- rep(na.strings, ncol(data))
    na.strings[names(data) == "Unique n"] <- ""
  }
  
  wb <- wb |>
    wb_add_data_multi_na(
      x = data, start_row = pm$rows$dat_start, start_col = 1, 
      na.strings = na.strings
    )  
  
  if (!is.null(pm$decked)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        x = pm$decked, start_row = pm$rows$decked_start, start_col = 1,
        col_names = FALSE, na.strings = ""
      ) |>
      openxlsx2::wb_add_cell_style(
        dims = openxlsx2::wb_dims(pm$rows$decked, pm$cols$all),
        horizontal = "center", wrap_text = TRUE
      ) |>
      openxlsx2::wb_set_row_heights(
        rows = pm$rows$decked, heights = 30
      ) |> 
      openxlsx2::wb_add_font(
        dims = openxlsx2::wb_dims(pm$rows$decked, pm$cols$all),
        bold = TRUE, italic = TRUE
      )      
    for (i in seq_along(pm$decked_fmt)) {
      fmt <- pm$decked_fmt[[i]]
      wb <- wb |> 
        openxlsx2::wb_add_border(
          dims = openxlsx2::wb_dims(i, fmt$rule, from_row = pm$rows$decked_start),
          top_border = NULL, left_border = NULL, right_border = NULL
        )
      for (cells in fmt$merge) {
        wb <- wb |>
          openxlsx2::wb_merge_cells(
            dims = openxlsx2::wb_dims(i, cells, from_row = pm$rows$decked_start)
          )
      }
    }
  }
  if (length(pm$header)) {
    wb <- wb |>
      openxlsx2::wb_add_data(
        x = pm$header, start_row = 1, start_col = 1, na.strings = ""
      ) |>
      openxlsx2::wb_add_font(
        dims = openxlsx2::wb_dims(pm$rows$header, 1), bold = TRUE
      )
  }

  for (chunk in params$hyperlinks) {
    wb <- wb |>
      openxlsx2::wb_add_formula(
        x = chunk$hl, start_col = 1, start_row = min(chunk$row)
      )
  }
  
  # Set column widths and freeze panes
  wb <- wb |>
    openxlsx2::wb_set_col_widths(cols = pm$cols$all, widths = "auto") |>
    openxlsx2::wb_freeze_pane(
      first_active_row = pm$rows$dat_start + 1,  first_active_col = 2
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
                              hyperlinks = TRUE,
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
  if (incl_date) cb_date <- glue_chr("Codebook generated {Sys.Date()}")
  sheet_nms <- list(
    overview = "Overview", num = "Summary - Numeric", 
    cat = "Summary - Categorical", txt = "Summary - Text",
    num_grp = "Grouped Summary - Numeric", cat_grp = "Grouped Summary - Categorical"
  )
  headers <- list(
    overview = c(dataset_name, cb_dims, cb_date),
    num = c(dataset_name, "Numeric variables summary"),
    cat = c(dataset_name, "Categorical variables summary"),
    txt = c(dataset_name, "Text variables summary")
  )
  
  params <- list()
 
  params$overview <- cb |>
    cb_format_names() |>
    cb_prep_sheet_data(
      sheet_name = sheet_nms$overview, header = headers$overview, 
      cols_pct = "Missing"
    )
  
  if (!is.null(summaries$num)) {
    params$num <- summaries$num |>
      cb_format_names() |>
      cb_prep_sheet_data(
        sheet_name = sheet_nms$num, header = headers$num, 
        cols_pct = "Valid %", cols_int = "Valid n"
      )
  }
  
  if (!is.null(summaries$cat)) {
    rows_border_by <- rows_sub_border_by <- NULL
    if (attr(summaries$cat, "detail_missing")) {
      summaries$cat <- cb_valid_miss_col(summaries$cat)
      rows_border_by <- "Name"
      rows_sub_border_by <- "Valid / Missing"
    }
    summaries$cat <- cb_format_names(summaries$cat, attrs = "id_cols")
    cols_pct <- untidyselect(summaries$cat, tidyselect::starts_with("%"))
    id_cols <- attr(summaries$cat, "id_cols")
    clear_repeats <- c(
      setdiff(id_cols, "Value"), 
      intersect("Valid / Missing", names(summaries$cat))
    )
    params$cat <- summaries$cat |>
      cb_prep_sheet_data(
        sheet_name = sheet_nms$cat, header = headers$cat, 
        cols_pct = cols_pct, cols_int = "n",
        clear_repeats = clear_repeats,
        rows_border_by = rows_border_by,
        rows_sub_border_by = rows_sub_border_by
      )
  }
  
  if (!is.null(summaries$txt)) {
    rows_border_by <- rows_sub_border_by <- NULL
    if (attr(summaries$txt, "detail_missing")) {
      summaries$txt <- cb_valid_miss_col(summaries$txt)
      rows_border_by <- "Name"
      rows_sub_border_by <- "Valid / Missing"
    }
    summaries$txt <- cb_format_names(summaries$txt, attrs = "id_cols")
    cols_pct <- untidyselect(summaries$txt, tidyselect::starts_with("%"))
    id_cols <- attr(summaries$txt, "id_cols")
    clear_repeats <- c(
      setdiff(id_cols, "Value"), 
      intersect("Valid / Missing", names(summaries$txt)),
      "Unique n"
    )
    params$txt <- summaries$txt |>
      cb_prep_sheet_data(
        sheet_name = sheet_nms$txt, header = headers$txt, 
        cols_pct = cols_pct, cols_int = c("Unique n", "n"),
        clear_repeats = clear_repeats,
        rows_border_by = rows_border_by,
        rows_sub_border_by = rows_sub_border_by
      )
  }
  
  if (!is.null(summaries$num_grp)) {
    group_by <- attr(summaries$num_grp, "group_by")
    summaries$num_grp <- summaries$num_grp |>
      cb_format_names(skip = group_by, attrs = "id_cols")
    clear_repeats <- c(
      attr(summaries$num_grp, "id_cols"), attr(summaries$num_grp, "group_rows")
    )
    sheet_nms$num_grp <- paste0("Grouped ", sheet_nms$num)
    headers$num_grp <- c(headers$num, paste0("By ", toString(group_by)))
    params$num_grp <- summaries$num_grp |>
      cb_prep_sheet_data(
        sheet_name = sheet_nms$num_grp, header = headers$num_grp, 
        cols_pct = "Valid %", cols_int = "Valid n",
        clear_repeats = clear_repeats
      )
    }

  if (!is.null(summaries$cat_grp)) {
    group_by <- attr(summaries$cat_grp, "group_by")
    summaries$cat_grp <- summaries$cat_grp |>
      cb_format_names(skip = group_by, attrs = "id_cols")
    cols_pct <- untidyselect(summaries$cat_grp, tidyselect::starts_with("%"))
    group_rows <- attr(summaries$cat_grp, "group_rows")
    id_cols <- attr(summaries$cat_grp, "id_cols")
    clear_repeats <- c(setdiff(id_cols, "Value"), group_rows)
    if (!is.null(group_rows)) {
      attr(summaries$cat_grp, "group_rows") <- c(group_rows, "Value")
      attr(summaries$cat_grp, "id_cols") <- setdiff(id_cols, "Value")
    }
    sheet_nms$cat_grp <- paste0("Grouped ", sheet_nms$cat)
    headers$cat_grp <- c(headers$cat, paste("By ", toString(group_by)))
    params$cat_grp <- summaries$cat_grp |>
      cb_prep_sheet_data(
        sheet_name = sheet_nms$cat_grp, header = headers$cat_grp,
        cols_pct = cols_pct, cols_int = "n",
        clear_repeats = clear_repeats
      )
  }
  
  if (getOption("lighthouse.codebook.nonresponse", FALSE)) {
    params <- label_nonresponse(params)
  }
  
  if (hyperlinks) params <- var_name_hyperlinks(params)

  opts <- options(
    openxlsx2.minWidth = min_col_width,
    openxlsx2.maxWidth = max_col_width
  )
  on.exit(options(opts))
    
  # initialize workbook 
  wb <- openxlsx2::wb_workbook()
  for (sheet in names(params)) {
    wb <- cb_write_sheet(
      wb, data = params[[sheet]]$data, params = params[[sheet]]
    )
  }
  
  openxlsx2::wb_save(wb, file, overwrite = overwrite)

  invisible(file)
}

