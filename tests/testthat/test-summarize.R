test_that("cb_summarize_numeric returns expected columns and NULL when no numeric vars", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  num <- lighthouse.codebook::cb_summarize_numeric(cb)
  expect_true(all(c("name", "valid_n", "valid_pct", "mean", "SD") %in% names(num)))

  dat_chr <- tibble::tibble(a = c("x", "y", NA), b = c("m", "m", "n"))
  cb_chr <- lighthouse.codebook::cb_create(dat_chr)
  expect_warning(
    out <- lighthouse.codebook::cb_summarize_numeric(cb_chr, warn_if_none = TRUE),
    "No numeric variables"
  )
  expect_null(out)
})

test_that("cb_summarize_categorical toggles detailed missing columns and supports grouping", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .user_missing = cat_code ~ c(Skipped = 2)
  )

  cat_detail <- lighthouse.codebook::cb_summarize_categorical(cb, detail_missing = TRUE)
  expect_true("is_missing" %in% names(cat_detail))
  expect_true("pct_of_missing" %in% names(cat_detail))

  cat_simple <- lighthouse.codebook::cb_summarize_categorical(cb, detail_missing = FALSE)
  expect_false("is_missing" %in% names(cat_simple))
  expect_false("pct_of_missing" %in% names(cat_simple))

  cat_group <- lighthouse.codebook::cb_summarize_categorical(cb, group_by = mh_red)
  expect_true("mh_red" %in% names(cat_group))
  expect_gt(nrow(cat_group), 0)
})

test_that("cb_summarize_text truncates displayed values with n_text_vals", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  txt <- lighthouse.codebook::cb_summarize_text(cb, n_text_vals = 1, detail_missing = FALSE)

  txt_note <- txt[txt$name == "txt_note", ]
  expect_true(any(grepl("other values", txt_note$value, fixed = TRUE)))
})

