#' Generate a codebook object from REDCap data
#' 
#' @description
#' `cb_create_redcap()` builds an object of class `"li_codebook"` from a dataset and
#' corresponding codebook exported from REDCap. The resulting object can be used
#' to write an Excel workbook with variable and data summaries (using [`cb_write()`]),
#' extract processed data ([`cb_get_data()`]), or generate dataset summaries ([`cb_summarize_numeric()`],
#' [`cb_summarize_categorical()`], [`cb_summarize_text()`]).
#'
#' This variant of [`cb_create()`] includes functionality specific to REDCap data
#' and metadata, including:
#' - Defaults for typical REDCap metadata column names
#' - Includes form field by default
#' - Unpacking, labelling, and optional missing propagation for checkbox data
#' - Optional coercion for character variables marked as "integer" in `metedata$text_validation_type_or_show_slider_number`
#' 
#' All of these behaviors can be controlled using the `.options` argument.
#' 
#' @inheritParams cb_create
#' @param data A data frame exported or retrieved from REDCap.
#' @param metadata A data frame containing the REDCap codebook associated with `data`.
#' @param ... Additional columns from `metadata` to preserve in the final codebook.
#'   New names can be assigned by passing named arguments. Columns for variable
#'   name, form, variable label, and value labels are included by default.
#' @param .user_missing A formula or list of formulas specifying user missing values.
#'   Formulas should specify variables on the left-hand side (as variable names
#'   or [tidyselect][dplyr_tidy_select] expressions), and missing values on the
#'   right-hand side. If left-hand side is omitted, defaults to `tidyselect::everything()`.
#'   See "Specifying user missing values" in [`cb_create()`] documentation  for examples.
#' @param .options Additional options to use for codebook creation. Must be the result 
#'   from a call to `cb_create_redcap_options()` or `cb_create_options()`. See `?cb_create_redcap_options` 
#'   for available options.
#'
#' @return
#' An `"li_codebook"` object, consisting of a tibble summarizing the passed
#' dataset and attributes containing additional metadata. The tibble includes columns:
#' - `name`: variable name
#' - `form`: form name
#' - `type`: column containing simplified variable type
#' - `class`: optional column containing class(es) of each variable
#' - `label_stem`: optional column containing variable label stems, if any variables
#'   are specified in `.split_var_labels`
#' - `label`: variable label
#' - `values`: values, with labels if applicable
#' - `user_missing`: optional column showing user missing values, with labels 
#'   if applicable. By default, this column is included only if user missings 
#'   are specified for at least one variable. This behavior can be changed using 
#'   the `user_missing_col` argument to `cb_create_options()`.
#' - `missing`: proportion missing
#' - additional columns if specified in `...`
#'
#' @section Checkbox data handling:
#' ## Value labels
#' Data from REDCap checkboxes yields one variable in the dataset for each response
#' option. By default, these will be labelled generically with `"Yes"` or `"No"`.
#' For example, consider a checkbox group with options "In the past year," "More than a
#' year ago," and "Never," corresponding to variables `chk_var1___0`, `chk_var1___1`,
#' and `chk_var1___2`. By default, all of these will be given the same value labels:
#'   - `chk_var1___0`, `chk_var1___1`, `chk_var1___2`: 0 = "No"; 1 =  "Yes". 
#' This behavior can be changed by setting `checkbox_resp_values = TRUE` in `cb_create_options()`. 
#' In this case, response-specific labels from `metadata` will be used, so that 
#' each variable will have unique labels:
#'   - `chk_var1___0`: 0 = "Not selected," 1 = "In the past year"
#'   - `chk_var1___1`: 0 = "Not selected," 1 = "More than a year ago"
#'   - `chk_var1___2`: 0 = "Not selected," 0 = "Never"
#'
#' ## Missing value propagation
#' By default, missing values in a checkbox group will be propagated to all variables 
#' in the group. For example, consider a checkbox group with options "Pregnant," 
#' "Not pregnant," and "Not applicable," corresponding to variables `chk_preg_0___0`, 
#' `chk_preg_0___1`, and `chk_preg_0____9`, and assuming that `-9` is specified 
#' as a user missing value. By default, `chk_preg_0___0` and `chk_preg_0___1` will 
#' be set to `-9` if `chk_preg_0____9` is `1`. This behavior can be overridden by 
#' setting `propagate_checkbox_missings = FALSE` in `cb_create_options()`, in which 
#' case no values will be changed.
#'
#' @export
cb_create_redcap <- function(data,
                             metadata,
                             ...,
                             .user_missing = NULL,
                             .split_var_labels = NULL,
                             .options = cb_create_redcap_options()) {
  check_options(.options, redcap = TRUE)
  meta <- meta_expand_checkboxes_rc(
    metadata, data,
    name = !!.options$name, type = !!.options$type
  )
  cb <- data |>
    cb_init(
      meta,
      meta_var_name = !!.options$name, meta_var_label = !!.options$var_label,
      meta_val_labels = !!.options$val_labels, form = !!.options$form, ...,
      ..rc_type = !!.options$type,
      ..rc_validate_type = text_validation_type_or_show_slider_number,
    )
  if (.options$coerce_integers) cb <- cb_coerce_integers_rc(cb)
  cb$..rc_validate_type <- NULL
  cb <- cb |>
    cb_clean_fields(
      rmv_html = .options$rmv_html, 
      rmv_line_breaks = .options$rmv_line_breaks
    ) |>
    cb_user_missings(
      user_missing = .user_missing,
      incompatible = .options$user_missing_incompatible
    ) |>
    cb_add_lookups(
      sep1 = .options$val_labs_sep1, 
      sep2 = .options$val_labs_sep2
    ) |>
    cb_relabel_checkboxes_rc(use_resp_values = .options$checkbox_resp_values)
  if ("form" %in% names(cb)) cb <- cb_complete_label_rc(cb)
  if (.options$propagate_checkbox_missings) {
    cb <- cb_propagate_user_missing_checkboxes_rc(cb)
  }
  cb |>
    cb_label_data(conflict = .options$user_missing_conflict) |>
    cb_zap_data() |>
    cb_add_dims() |>
    cb_add_val_labels_col(user_missing_col = .options$user_missing_col) |>
    cb_add_type_col(
      include_r_classes = .options$include_r_classes,
      include_types = .options$include_types
    ) |>
    cb_add_missing_col() |>
    cb_split_labels_col(split_var_labels = rlang::enexpr(.split_var_labels)) |> 
    dplyr::relocate(any_of(c("form", "type", "class")), .after = name)
}

#' @rdname cb_create_options
#' 
#' @param name,var_label,val_labels,type For REDCap data, columns in `metadata` containing variable
#'   name, variable label, value labels, and variable type, respectively.
#' @param form For REDCap data, column in `metadata` containing form names. (Set to `NULL` to omit.)
#' @param val_labs_sep1,val_labs_sep2 For REDCap data, regex patterns separating value labels
#'   in `metadata`. `val_labs_sep1` separates values from labels, and `val_labs_sep2`
#'   separates value/label pairs from one another. e.g., if value labels are in 
#'   the format `"1, First label|2, Second label"`, set `val_labs_sep1` to `","` 
#'   and `val_labs_sep2` to `"\\|"`.
#' @param coerce_integers For REDCap data, should variables listed as "integer" in `metedata$text_validation_type_or_show_slider_number` 
#'   be coerced to integer?
#' @param checkbox_resp_values For REDCap data, should checkbox values use labels in `metadata` (`TRUE`) 
#'   or "Yes" / "No" (`FALSE`)? See "Checkbox data handling"  on the `cb_create_redcap()` 
#'   help page.
#' @param propagate_checkbox_missings For REDCap data, should user missing values in a checkbox group 
#'   be propagated across all variables in the group? See "Checkbox data handling" 
#'   on the `cb_create_redcap()` help page.
#' 
#' @export
cb_create_redcap_options <- function(
    ...,
    include_types = TRUE,
    include_r_classes = FALSE,
    rmv_html = TRUE,
    rmv_line_breaks = TRUE,
    user_missing_col = c("if_any", "yes", "no"),
    user_missing_conflict = c("val_label", "missing_label"),
    user_missing_incompatible = c("ignore", "warn", "error"),
    name = field_name,
    var_label = field_label,
    val_labels = select_choices_or_calculations,
    type = field_type,
    form = form_name,
    val_labs_sep1 = ", ",
    val_labs_sep2 = "\\|",
    coerce_integers = TRUE,
    checkbox_resp_values = FALSE,
    propagate_checkbox_missings = TRUE) {
  rlang::check_dots_empty()
  out <- list(
    include_types = include_types, include_r_classes = include_r_classes,
    rmv_html = rmv_html, rmv_line_breaks = rmv_line_breaks,
    user_missing_col = user_missing_col,
    user_missing_conflict = user_missing_conflict,
    user_missing_incompatible = user_missing_incompatible, 
    name = rlang::enquo(name), var_label = rlang::enquo(var_label), 
    val_labels = rlang::enquo(val_labels), type = rlang::enquo(type), 
    form = rlang::enquo(form), val_labs_sep1 = val_labs_sep1, 
    val_labs_sep2 = val_labs_sep2, coerce_integers = coerce_integers,
    checkbox_resp_values = checkbox_resp_values,
    propagate_checkbox_missings = propagate_checkbox_missings
  )
  structure(out, class = "cb_create_redcap_options")
}

meta_expand_checkboxes_rc <- function(meta, data, name, type) {
  name_chr <- as.character(rlang::ensym(name))
  type_chr <- as.character(rlang::ensym(type))
  if (!("checkbox" %in% meta[[type_chr]])) return(meta)
  datanames <- names(data)
  meta <- dplyr::rename(meta, ..name = {{ name }}, ..type = {{ type }})
  checkbox_names <- meta |>
    dplyr::filter(..type == "checkbox") |>
    dplyr::select(..name) |>
    dplyr::reframe(
      .chk_name = datanames[
        stringr::str_starts(datanames, stringr::str_c(..name, "___"))
      ],
      .by = ..name
    )
  meta |>
    dplyr::left_join(checkbox_names, dplyr::join_by(..name)) |>
    dplyr::mutate(
      # .chk_name_stem = ifelse(!is.na(.chk_name), field_name, NA),
      ..name = dplyr::coalesce(.chk_name, ..name),
      .keep = "unused"
    ) |>
    dplyr::rename("{name_chr}" := ..name, "{type_chr}" := ..type)
}

cb_coerce_integers_rc <- function(cb) {
  data <- attr(cb, "data")
  data
  integers <- cb$name[cb$..rc_validate_type %in% "integer"]
  data <- data |>
    dplyr::mutate(dplyr::across(any_of(integers), as.integer))
  set_attrs(cb, data = data)
}

cb_relabel_checkboxes_rc <- function(cb, use_resp_values = FALSE) {
  chk_name_to_val <- function(name, sep = "___", neg_char = "_") {
    name |>
      stringr::str_extract(glue_chr("(?<={sep}).+")) |>
      stringr::str_replace(neg_char, "-")
  }
  chk_name_to_stem <- function(name, sep = "___") {
    stringr::str_remove(name, glue_chr("{sep}.+"))
  }
  chk_lookup_to_name <- function(stem, val, sep = "___", neg_char = "_") {
    stringr::str_c(stem, sep, stringr::str_replace(val, "-", neg_char))
  }
  propagation_info <- function(name, val, miss, lookup) {
    if (!(val %in% miss)) {
      return(NULL)
    }
    val <- unname(miss[miss == val])
    vars <- chk_lookup_to_name(
      stem = chk_name_to_stem(name),
      val = lookup[lookup != val]
    )
    list(flag = name, val = val, vars = vars)
  }
  make_01_labs <- function(use_resp_values, source_1) {
    if (use_resp_values) {
      labs <- c("Not selected", source_1)
    } else {
      labs <- c("No", "Yes")
    }
    setNames(0:1, labs)
  }
  if (!("checkbox" %in% cb$..rc_type)) return(dplyr::select(cb, !..rc_type))
  cb_chk <- cb |>
    dplyr::filter(..rc_type == "checkbox") |>
    dplyr::mutate(
      chk_value = chk_name_to_val(name),
      vals_by_label = attr(cb, "vals_by_label")[name],
      labs_by_value = attr(cb, "labs_by_value")[name],
      user_missing = attr(cb, "user_missing")[name]
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # is_miss = chk_value %in% user_missing,
      miss_propagate = list(propagation_info(
        name, chk_value, user_missing, vals_by_label
      )),
      chk_label = labs_by_value[chk_value],
      user_missing = list(user_missing[user_missing != chk_value]),
      lab01 = list(make_01_labs(use_resp_values, chk_label)),
      vals_by_label = list(c(
        lab01,
        vals_by_label[vals_by_label %in% user_missing]
      )),
      labs_by_value = list(setNames(names(vals_by_label), vals_by_label)),
      
      label = stringr::str_c(label, chk_label, sep = " - ")
    ) |>
    dplyr::ungroup()

  cb <- cb |>
    dplyr::rows_update(dplyr::select(cb_chk, name, label), by = "name") |>
    dplyr::select(!..rc_type)

  for (att in c("vals_by_label", "labs_by_value", "user_missing")) {
    att_out <- attr(cb, att)
    att_chk <- purrr::compact(setNames(cb_chk[[att]], cb_chk$name))
    att_out[names(att_chk)] <- att_chk
    attr(cb, att) <- att_out
  }
  attr(cb, "miss_propagate") <- purrr::compact(
    setNames(cb_chk$miss_propagate, cb_chk$name)
  )
  cb
}

cb_complete_label_rc <- function(cb) {
  stopifnot("form" %in% names(cb))
  compl_info <- cb |>
    dplyr::distinct(form) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      name = stringr::str_c(form, "_complete"),
      label = stringr::str_c(form, " completion status")
    ) |>
    dplyr::semi_join(cb, dplyr::join_by(name))
  cb <- cb |>
    dplyr::rows_patch(compl_info, by = "name")
  compl_nms <- compl_info$name
  vals_by_label <- attr(cb, "vals_by_label")
  vals_by_label[compl_nms] <- rep(
    list(c(Incomplete = 0, Unverified = 1, Complete = 2)),
    length(compl_nms)
  )
  attr(cb, "vals_by_label") <- vals_by_label
  labs_by_value <- attr(cb, "labs_by_value")
  labs_by_value[compl_nms] <- rep(
    list(c("0" = "Incomplete", "1" = "Unverified", "2" = "Complete")),
    length(compl_nms)
  )
  attr(cb, "labs_by_value") <- labs_by_value
  cb
}

cb_propagate_user_missing_checkboxes_rc <- function(cb) {
  mp <- attr(cb, "miss_propagate")
  if (is.null(mp)) return(cb)
  data <- attr(cb, "data")
  for (mpi in mp) {
    for (var in mpi$vars) {
      data[[var]][data[[mpi$flag]] == 1] <- mpi$val
    }
  }
  set_attrs(cb, data = data)
}
