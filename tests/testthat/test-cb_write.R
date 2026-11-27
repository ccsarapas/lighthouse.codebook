test_that("`cb_write()` writes workbook and includes core summary sheets", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)
  out <- cb_write(cb, file = out_file, overwrite = TRUE)

  expect_identical(out, out_file)
  expect_true(file.exists(out_file))

  wb <- openxlsx2::wb_load(out_file)
  sheets <- openxlsx2::wb_get_sheet_names(wb)

  expect_true(all(c(
    "Overview",
    "Summary - Numeric",
    "Summary - Categorical",
    "Summary - Text"
  ) %in% sheets))
})

test_that("`cb_write()` preserves split labels in the text summary (#40)", {
  data <- tibble::tibble(
    mood_morning = c("Calm", "Tense"),
    mood_evening = c("Tense", "Calm")
  )
  metadata <- tibble::tibble(
    name = names(data),
    label = c("Mood: Morning", "Mood: Evening")
  )
  cb <- cb_create(
    data,
    metadata,
    .val_labels = NULL,
    .split_var_labels = tidyselect::starts_with("mood_")
  )

  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)
  expect_identical(cb_write(cb, file = out_file), out_file)

  wb <- openxlsx2::wb_load(out_file)
  txt <- openxlsx2::wb_to_df(wb, sheet = "Summary - Text", start_row = 2)
  expect_true("Label Stem" %in% names(txt))
  label_rows <- match(c("Morning", "Evening"), txt$Label)
  expect_identical(trimws(txt[["Label Stem"]][label_rows]), c("Mood:", "Mood:"))
})

test_that("`cb_write()` `stats_numeric` controls numeric summary columns", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)
  cb_write(
    cb,
    file = out_file,
    stats_numeric = list(mean = mean),
    overwrite = TRUE
  )

  wb <- openxlsx2::wb_load(out_file)
  num <- openxlsx2::wb_to_df(wb, sheet = "Summary - Numeric", start_row = 2)

  expect_true("Mean" %in% names(num))
  expect_false("SD" %in% names(num))
})

test_that("`cb_write()` `group_by` adds grouped summary sheets", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)
  cb_write(
    cb,
    file = out_file,
    group_by = mh_red,
    overwrite = TRUE
  )

  wb <- openxlsx2::wb_load(out_file)
  sheets <- openxlsx2::wb_get_sheet_names(wb)

  expect_true(all(c(
    "Grouped Summary - Numeric",
    "Grouped Summary - Categorical"
  ) %in% sheets))
})
