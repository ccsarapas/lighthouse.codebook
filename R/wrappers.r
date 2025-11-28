

gen_codebook_rc <- function(data, 
                            metadata, 
                            ...,
                            .user_missing = NULL, 
                            .val_labs_sep1 = ", ", 
                            .val_labs_sep2 = "\\|",
                            .rmv_html = !name, 
                            .rmv_line_breaks = !name,
                            .checkbox_resp_values = FALSE,
                            .separate_missings = c("if_any", "yes", "no"),
                            .user_missing_conflict = c("value_label", "missing_label")) {
  .separate_missings <- match.arg(.separate_missings)
  .user_missing_conflict <- match.arg(.user_missing_conflict)
  meta <- meta_expand_checkboxes_rc(metadata, data)
  data |>
    cb_init(
      meta,
      meta_var_name = field_name, meta_var_label = field_label,
      meta_val_labels = select_choices_or_calculations, form = form_name, ...,
      ..rc_type = field_type,
      ..rc_validate_type = text_validation_type_or_show_slider_number,
    ) |>
    cb_coerce_integers_rc() |>
    cb_clean_fields(
      rmv_html = {{ .rmv_html }}, 
      rmv_line_breaks = {{ .rmv_line_breaks }}
    ) |>
    cb_user_missings(user_missing = .user_missing) |>
    cb_add_lookups(sep1 = .val_labs_sep1, sep2 = .val_labs_sep2) |>
    cb_relabel_checkboxes_rc(use_resp_values = .checkbox_resp_values) |>
    cb_complete_label_rc() |>
    cb_propagate_user_missing_checkboxes_rc() |>
    cb_label_data(conflict = .user_missing_conflict) |>
    cb_zap_data() |>
    cb_add_dims() |>
    cb_add_val_labels(separate_missings = .separate_missings) |>
    cb_add_types() |>
    cb_add_missing() |> 
    dplyr::relocate(form, type, .after = name)
}

write_codebook <- function(cb, 
                           file, 
                           dataset_name,
                           incl_date = TRUE,
                           incl_dims = TRUE,
                           detail_missing = TRUE,
                           group_by = NULL,
                           overwrite = TRUE) {
  summaries <- cb_gen_summaries(
    cb, detail_missing = detail_missing, group_by = {{ group_by }}
  )
  cb_write_codebook(
    cb, summaries,
    file = file, dataset_name = dataset_name, incl_date = incl_date, 
    incl_dims = incl_dims, detail_missing = detail_missing, 
    group_by = {{ group_by }}, overwrite = overwrite
  )
}
