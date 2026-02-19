test_that("cb_get_data factors format converts labelled vars and zaps user missings", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .user_missing = num_score ~ 99
  )

  out <- lighthouse.codebook::cb_get_data(cb, format = "factors")

  expect_s3_class(out, "data.frame")
  expect_true(is.factor(out$cat_code))
  expect_true(any(is.na(out$num_score)))
  expect_false(any(out$num_score == 99, na.rm = TRUE))
})

test_that("cb_get_data haven format returns labelled vectors with missing metadata", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; ",
    .user_missing = num_score ~ 99
  )

  out <- lighthouse.codebook::cb_get_data(cb, format = "haven")

  expect_true("haven_labelled_spss" %in% class(out$cat_code))
  expect_true("haven_labelled_spss" %in% class(out$num_score))
  expect_true(99 %in% labelled::na_values(out$num_score))
})

test_that("cb_get_data rejects deprecated format values", {
  fx <- fixture_core()
  cb <- lighthouse.codebook::cb_create(data = fx$data)

  expect_error(
    lighthouse.codebook::cb_get_data(cb, format = "values"),
    "no longer supported"
  )
})
