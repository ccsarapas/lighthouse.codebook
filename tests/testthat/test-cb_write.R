test_that("cb_write writes workbook and includes core summary sheets", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)
  out <- lighthouse.codebook::cb_write(cb, file = out_file, overwrite = TRUE)

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

test_that("cb_write grouped mode adds grouped summary sheets", {
  fx <- fixture_core()

  cb <- lighthouse.codebook::cb_create(
    data = fx$data,
    metadata = fx$metadata,
    .val_labs_sep1 = " = ",
    .val_labs_sep2 = "; "
  )

  out_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_file), add = TRUE)
  lighthouse.codebook::cb_write(
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
