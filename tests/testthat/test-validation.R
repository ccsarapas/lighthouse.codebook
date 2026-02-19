test_that("cb_create and cb_create_redcap enforce options class", {
  fx <- fixture_core()

  expect_error(
    lighthouse.codebook::cb_create(
      data = fx$data,
      metadata = fx$metadata,
      .options = lighthouse.codebook::cb_create_redcap_options()
    ),
    "Did you mean to call"
  )

  rc <- fixture_redcap()
  expect_error(
    lighthouse.codebook::cb_create_redcap(
      data = rc$data,
      metadata = rc$metadata,
      .options = lighthouse.codebook::cb_create_options()
    ),
    "must be created from"
  )
})

test_that("cb_create validates .user_missing argument type", {
  fx <- fixture_core()

  expect_error(
    lighthouse.codebook::cb_create(
      data = fx$data,
      metadata = fx$metadata,
      .val_labs_sep1 = " = ",
      .val_labs_sep2 = "; ",
      .user_missing = c(98, 99)
    ),
    "must be a formula or list of formulas"
  )
})

test_that("cb_write validates group_rows arguments", {
  fx <- fixture_core()
  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  expect_error(
    lighthouse.codebook::cb_write(
      cb,
      file = tempfile(fileext = ".xlsx"),
      group_rows = mh_red
    ),
    "group_by"
  )

  expect_error(
    lighthouse.codebook::cb_write(
      cb,
      file = tempfile(fileext = ".xlsx"),
      group_by = mh_red,
      group_rows = cat_code
    ),
    "must also be included in"
  )
})
