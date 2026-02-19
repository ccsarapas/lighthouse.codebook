test_that("cb_create_spss smoke: imports labels and SPSS user missing metadata", {
  dat <- fixture_spss()

  cb <- lighthouse.codebook::cb_create_spss(dat)

  expect_s3_class(cb, "li_codebook")
  expect_true("spss_var" %in% cb$name)

  vals <- cb$values[cb$name == "spss_var"]
  expect_match(vals, "\\[1\\].*Yes")
  expect_match(vals, "\\[2\\].*No")

  expect_true(9 %in% unname(attr(cb, "user_missing")$spss_var))

  dat_haven <- lighthouse.codebook::cb_get_data(cb, format = "haven")
  expect_true("Refused" %in% names(labelled::val_labels(dat_haven$spss_var)))
  expect_true(9 %in% labelled::na_values(dat_haven$spss_var))
})

test_that("cb_create_spss + cb_get_data factors zaps user missing values", {
  dat <- fixture_spss()
  cb <- lighthouse.codebook::cb_create_spss(dat)

  out <- lighthouse.codebook::cb_get_data(cb, format = "factors")

  expect_true(is.factor(out$spss_var))
  expect_true(any(is.na(out$spss_var)))
})
