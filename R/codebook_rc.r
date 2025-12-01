
## `field_name` and `field_type` are hard-coded -- do they always have these names?
meta_expand_checkboxes_rc <- function(meta, data) {
  datanames <- names(data)
  checkbox_names <- meta |>
    dplyr::filter(field_type == "checkbox") |>
    dplyr::select(field_name) |>
    dplyr::reframe(
      .chk_name = datanames[
        stringr::str_starts(datanames, stringr::str_c(field_name, "___"))
      ],
      .by = field_name
    )
  meta |>
    dplyr::left_join(checkbox_names, dplyr::join_by(field_name)) |>
    dplyr::mutate(
      # .chk_name_stem = ifelse(!is.na(.chk_name), field_name, NA),
      field_name = dplyr::coalesce(.chk_name, field_name),
      .keep = "unused"
    )
}

cb_coerce_integers_rc <- function(cb) {
  data <- attr(cb, "data")
  data
  integers <- cb$name[cb$..rc_validate_type %in% "integer"]
  data <- data |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(integers), as.integer))
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
  data <- attr(cb, "data")
  mp <- attr(cb, "miss_propagate")
  for (mpi in mp) {
    for (var in mpi$vars) {
      data[[var]][data[[mpi$flag]] == 1] <- mpi$val
    }
  }
  set_attrs(cb, data = data)
}
