spss_issue32_vars <- c(
  "num_na_vals",
  "num_na_range",
  "cat_na_vals_unlabelled",
  "cat_na_vals_labelled",
  "cat_na_range",
  "txt_na_vals"
)

spss_issue32_cat_vars <- c(
  "cat_na_vals_unlabelled",
  "cat_na_vals_labelled",
  "cat_na_range"
)

test_that("`cb_create_spss()` imports variable labels and value labels for issue #32 fixture", {
  dat <- fixture_spss()
  cb <- suppressWarnings(cb_create_spss(dat))

  expect_s3_class(cb, "li_codebook")
  expect_identical(cb$name, names(dat))

  expected_labels <- c(
    num_na_vals = "Numeric NA values",
    num_na_range = "Numeric NA range",
    cat_na_vals_unlabelled = "Categorical NA values (unlabelled missing)",
    cat_na_vals_labelled = "Categorical NA values (labelled missing)",
    cat_na_range = "Categorical NA range",
    txt_na_vals = "Text NA values"
  )

  expect_mapequal(setNames(cb$label, cb$name), expected_labels)

  expect_match(cb$values[cb$name == "cat_na_vals_unlabelled"], "\\[1\\].*Yes")
  expect_match(cb$values[cb$name == "cat_na_vals_unlabelled"], "\\[2\\].*No")
  expect_match(cb$user_missings[cb$name == "cat_na_vals_labelled"], "\\[-9\\].*Refused")

  expect_mapequal(
    attr(cb, "vals_by_label")$cat_na_vals_unlabelled,
    c(Yes = 1, No = 2)
  )
})

test_that("`cb_create_spss()` imports user missings from `na_values` and `na_range` (issue #32)", {
  dat <- fixture_spss()
  expect_warning(
    cb <- cb_create_spss(dat),
    "User missing ranges will be treated as discrete user missing values"
  )

  attr_user_miss <- attr(cb, "user_missing")
  attr_user_miss_names <- names(attr_user_miss)
  if (is.null(attr_user_miss_names)) attr_user_miss_names <- character()

  expect_setequal(attr_user_miss_names, spss_issue32_vars)

  if (length(attr_user_miss_names)) {
    expect_in(-9, attr_user_miss$num_na_vals)
    expect_in(-9, attr_user_miss$num_na_range)
    expect_in(-9, attr_user_miss$cat_na_vals_unlabelled)
    expect_in(-9, attr_user_miss$cat_na_vals_labelled)
    expect_in(-9, attr_user_miss$cat_na_range)
    expect_in("-9", as.character(attr_user_miss$txt_na_vals))
  }

  expect_true("user_missings" %in% names(cb))
  expect_true(all(!is.na(cb$user_missings[match(spss_issue32_vars, cb$name)])))
})

test_that("`cb_summarize_categorical()` with `detail_missing = TRUE` flags SPSS user missings", {
  cb <- suppressWarnings(cb_create_spss(fixture_spss()))
  out <- cb_summarize_categorical(cb, detail_missing = TRUE)
  out_cat <- out[out$name %in% spss_issue32_cat_vars, ]

  missing_rows <- out_cat[out_cat$is_missing, c("name", "n", "pct_of_missing")]
  expect_setequal(unique(missing_rows$name), spss_issue32_cat_vars)
  expect_true(all(missing_rows$n == 1L))
  expect_true(all(missing_rows$pct_of_missing == 1))

  valid_rows <- out_cat[!out_cat$is_missing, ]
  expect_true(all(valid_rows$pct_of_valid == 0.5))

  refused <- out_cat[
    out_cat$name == "cat_na_vals_labelled" & grepl("Refused", out_cat$value, fixed = TRUE),
  ]
  expect_equal(nrow(refused), 1)
  expect_true(refused$is_missing)
  expect_equal(refused$n, 1L)
})

test_that("`cb_summarize_categorical()` with `detail_missing = FALSE` does not duplicate labelled user missings", {
  cb <- suppressWarnings(cb_create_spss(fixture_spss()))
  out <- cb_summarize_categorical(cb, detail_missing = FALSE)
  out_cat <- out[out$name %in% spss_issue32_cat_vars, ]

  missing_names <- out_cat$name[out_cat$value == "(Missing)"]
  expect_setequal(missing_names, spss_issue32_cat_vars)

  expect_false(any(
    out_cat$name == "cat_na_vals_labelled" & grepl("Refused", out_cat$value, fixed = TRUE)
  ))
})

test_that("`cb_summarize_text()` detailed missing rows keep SPSS user missing values", {
  cb <- suppressWarnings(cb_create_spss(fixture_spss()))
  out <- cb_summarize_text(cb, detail_missing = TRUE)

  miss <- out[out$name == "txt_na_vals" & out$is_missing, ]

  expect_equal(nrow(miss), 1)
  expect_false(is.na(miss$value))
  expect_match(miss$value, "-9", fixed = TRUE)
  expect_equal(miss$n, 1L)
  expect_equal(miss$pct_of_missing, 1)
})

test_that("`cb_create_spss()` + `cb_get_data()` factors zaps SPSS user missing values", {
  cb <- suppressWarnings(cb_create_spss(fixture_spss()))
  out <- cb_get_data(cb, format = "factors")

  expect_true(any(is.na(out$num_na_vals)))
  expect_true(any(is.na(out$num_na_range)))
  expect_true(any(is.na(out$cat_na_vals_unlabelled)))
  expect_true(any(is.na(out$cat_na_vals_labelled)))
  expect_true(any(is.na(out$cat_na_range)))
  expect_true(any(is.na(out$txt_na_vals)))

  expect_false(any(out$num_na_vals == -9, na.rm = TRUE))
  expect_false(any(out$num_na_range == -9, na.rm = TRUE))
  expect_false(any(out$txt_na_vals == "-9", na.rm = TRUE))
})
