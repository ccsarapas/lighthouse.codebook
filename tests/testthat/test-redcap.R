test_that("cb_create_redcap smoke: checkbox relabeling and missing propagation", {
  fx <- fixture_redcap()

  cb <- lighthouse.codebook::cb_create_redcap(
    data = fx$data,
    metadata = fx$metadata,
    .user_missing = ~ 9,
    .options = lighthouse.codebook::cb_create_redcap_options(
      checkbox_resp_values = TRUE,
      propagate_checkbox_missings = TRUE
    )
  )

  expect_s3_class(cb, "li_codebook")
  expect_true(all(c("q1___0", "q1___1", "q1___9") %in% cb$name))

  vals_0 <- cb$values[cb$name == "q1___0"]
  vals_9 <- cb$values[cb$name == "q1___9"]
  expect_match(vals_0, "Option A")
  expect_match(vals_9, "Not asked")

  dat_haven <- lighthouse.codebook::cb_get_data(cb, format = "haven")
  expect_equal(as.numeric(dat_haven$q1___0[3]), 9)
  expect_equal(as.numeric(dat_haven$q1___1[3]), 9)
  expect_true(9 %in% labelled::na_values(dat_haven$q1___0))
  expect_true(9 %in% labelled::na_values(dat_haven$q1___1))
})
