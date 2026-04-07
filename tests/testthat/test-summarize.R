test_that("`cb_summarize_numeric()` returns expected columns", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  num <- cb_summarize_numeric(cb)
  expect_true(all(c("name", "valid_n", "valid_pct", "mean", "SD") %in% names(num)))
})

test_that("`cb_summarize_numeric()` returns `NULL` when no numeric vars", {
  dat_chr <- data.frame(a = c("x", "y", NA), b = c("m", "m", "n"))
  cb_chr <- cb_create(dat_chr)
  expect_warning(out <- cb_summarize_numeric(cb_chr), "No numeric variables")
  expect_null(out)
})

test_that("`cb_summarize_numeric()` supports grouping", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  num_group <- cb_summarize_numeric(cb, group_by = mh_red)
  expect_true("mh_red" %in% names(num_group))
  expect_gt(nrow(num_group), 0)
})

test_that("`cb_summarize_numeric()` supports numeric grouping variables (issue #31)", {
  dat <- data.frame(
    grp = c(1, 2, 1, 2),
    num = c(1, 2, 3, 4)
  )

  cb <- cb_create(dat)

  num_group <- NULL
  expect_no_error(
    num_group <- cb_summarize_numeric(cb, group_by = grp)
  )

  if (!is.null(num_group)) expect_true("grp" %in% names(num_group))
})

test_that("`cb_summarize_numeric()` `stats` controls included statistic columns", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  num <- cb_summarize_numeric(
    cb,
    stats = list(mean = mean)
  )

  expect_true(all(c("valid_n", "valid_pct", "mean") %in% names(num)))
  expect_false("SD" %in% names(num))
  expect_false("median" %in% names(num))
})

test_that("`cb_summarize_numeric()` accepts non-default custom stats functions", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  num <- cb_summarize_numeric(
    cb,
    stats = list(q25 = \(x) quantile(x, 0.25, na.rm = TRUE))
  )

  expect_true("q25" %in% names(num))
  score <- num[num$name == "num_score", "q25", drop = TRUE]
  expect_equal(score, quantile(fx$data$num_score, 0.25, na.rm = TRUE))
})

test_that(
  "`cb_summarize_categorical()` `detail_missing` toggles detailed missing columns", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .user_missing = cat_code ~ c(Skipped = 2)
  )

  cat_detail <- cb_summarize_categorical(cb, detail_missing = TRUE)
  expect_true("is_missing" %in% names(cat_detail))
  expect_true("pct_of_missing" %in% names(cat_detail))

  cat_simple <- cb_summarize_categorical(cb, detail_missing = FALSE)
  expect_false("is_missing" %in% names(cat_simple))
  expect_false("pct_of_missing" %in% names(cat_simple))
})

test_that("`cb_summarize_categorical()` supports grouping", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .user_missing = cat_code ~ c(Skipped = 2)
  )

  cat_group <- cb_summarize_categorical(cb, group_by = mh_red)
  expect_true("mh_red" %in% names(cat_group))
  expect_gt(nrow(cat_group), 0)
})

test_that("`cb_summarize_text()` `detail_missing` toggles detailed missing columns", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .user_missing = txt_note ~ c(Skipped = "SKIP")
  )

  txt_detail <- cb_summarize_text(cb, detail_missing = TRUE)
  expect_true("is_missing" %in% names(txt_detail))
  expect_true("pct_of_missing" %in% names(txt_detail))

  txt_simple <- cb_summarize_text(cb, detail_missing = FALSE)
  expect_false("is_missing" %in% names(txt_simple))
  expect_false("pct_of_missing" %in% names(txt_simple))
})

test_that("`cb_summarize_text()` truncates displayed values with `n_text_vals`", {
  fx <- fixture_core()

  cb <- cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  txt <- cb_summarize_text(cb, n_text_vals = 1, detail_missing = FALSE)

  txt_note <- txt[txt$name == "txt_note", ]
  expect_true(any(grepl("other values", txt_note$value, fixed = TRUE)))
})

