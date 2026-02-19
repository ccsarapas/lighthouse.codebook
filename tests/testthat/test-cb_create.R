test_that("cb_create returns li_codebook and preserves variable order", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  expect_s3_class(cb, "li_codebook")
  expect_identical(cb$name, names(fx$data))
  expect_true(all(c("name", "type", "label", "values", "missing") %in% names(cb)))
})

test_that("cb_create parses metadata value labels and errors without separators", {
  fx <- fixture_core()

  expect_error(
    lighthouse.codebook::cb_create(data = fx$data, metadata = fx$metadata),
    "sep1"
  )

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  cat_vals <- cb$values[cb$name == "cat_code"]
  expect_match(cat_vals, "\\[1\\].*Yes")
  expect_match(cat_vals, "\\[2\\].*No")
})

test_that("cb_create handles user missing incompatibility according to options", {
  fx <- fixture_core()

  opts_warn <- lighthouse.codebook::cb_create_options(user_missing_incompatible = "warn")
  expect_warning(
    lighthouse.codebook::cb_create(
      data = fx$data,
      metadata = fx$metadata,
      .val_labs_sep1 = " = ",
      .val_labs_sep2 = "; ",
      .user_missing = event_date ~ as.Date("2024-01-01"),
      .options = opts_warn
    ),
    "not compatible"
  )

  opts_error <- lighthouse.codebook::cb_create_options(user_missing_incompatible = "error")
  expect_error(
    lighthouse.codebook::cb_create(
      data = fx$data,
      metadata = fx$metadata,
      .val_labs_sep1 = " = ",
      .val_labs_sep2 = "; ",
      .user_missing = event_date ~ as.Date("2024-01-01"),
      .options = opts_error
    ),
    "not compatible"
  )
})

test_that("cb_create split_var_labels creates label_stem and rejects overlaps", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .split_var_labels = tidyselect::starts_with("mh_")
  )

  expect_true("label_stem" %in% names(cb))
  mh_idx <- cb$name %in% c("mh_red", "mh_blue")
  expect_equal(length(unique(stats::na.omit(cb$label_stem[mh_idx]))), 1)
  expect_setequal(cb$label[mh_idx], c("Red", "Blue"))

  expect_error(
    lighthouse.codebook::cb_create(
      data = fx$data,
      metadata = fx$metadata,
      .val_labs_sep1 = " = ",
      .val_labs_sep2 = "; ",
      .split_var_labels = list(tidyselect::starts_with("mh_"), mh_red)
    ),
    "captured by more than one expression"
  )
})

