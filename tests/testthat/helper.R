fixture_core <- function() {
  data <- tibble::tibble(
    id = 1:6,
    num_score = c(10, 12, 99, NA, 15, 99),
    cat_code = c(1, 2, 1, 2, NA, 2),
    mh_red = c(1, 0, 1, 0, 1, 0),
    mh_blue = c(0, 1, 0, 1, 0, 1),
    txt_note = c("alpha", "beta", "alpha", NA, "gamma", "alpha"),
    event_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05", "2024-01-06"))
  )

  metadata <- tibble::tibble(
    name = names(data),
    label = c(
      "Record ID",
      "Numeric score",
      "Binary category",
      "Mood: Red",
      "Mood: Blue",
      "Free-text note",
      "Event date"
    ),
    val_labels = c(
      NA,
      NA,
      "1 = Yes; 2 = No",
      "0 = No; 1 = Yes",
      "0 = No; 1 = Yes",
      NA,
      NA
    )
  )

  list(data = data, metadata = metadata)
}

fixture_redcap <- function() {
  data <- tibble::tibble(
    q1___0 = c(1, 0, 0),
    q1___1 = c(0, 1, 0),
    q1___9 = c(0, 0, 1)
  )

  metadata <- tibble::tibble(
    field_name = "q1",
    field_label = "Select all that apply",
    select_choices_or_calculations = "0, Option A | 1, Option B | 9, Not asked",
    field_type = "checkbox",
    form_name = "form_a",
    text_validation_type_or_show_slider_number = NA_character_
  )

  list(data = data, metadata = metadata)
}

fixture_spss <- function() {
  vals <- c(1, 2, -9)

  tibble::tibble(
    num_na_vals = haven::labelled_spss(
      vals,
      na_values = -9,
      label = "Numeric NA values"
    ),
    num_na_range = haven::labelled_spss(
      vals,
      na_range = c(-9, -1),
      label = "Numeric NA range"
    ),
    cat_na_vals_unlabelled = haven::labelled_spss(
      vals,
      labels = c(Yes = 1, No = 2),
      na_values = -9,
      label = "Categorical NA values (unlabelled missing)"
    ),
    cat_na_vals_labelled = haven::labelled_spss(
      vals,
      labels = c(Yes = 1, No = 2, Refused = -9),
      na_values = -9,
      label = "Categorical NA values (labelled missing)"
    ),
    cat_na_range = haven::labelled_spss(
      vals,
      labels = c(Yes = 1, No = 2),
      na_range = c(-9, -1),
      label = "Categorical NA range"
    ),
    txt_na_vals = haven::labelled_spss(
      as.character(vals),
      na_values = "-9",
      label = "Text NA values"
    )
  )
}
