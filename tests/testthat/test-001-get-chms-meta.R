test_that("Participant meta loads successfully from clinic file and data directories", {
  # Skip if configs not found
  testthat::skip_if_not(nzchar(config::get("cycle7_agd_dir")))
  testthat::skip_if_not(nzchar(config::get("cycle7_clinic_file")))

  # Load participant meta
  meta <- get_chms_meta(
    clinic_file = config::get("cycle7_clinic_file"),
    agd_dir = config::get("cycle7_agd_dir")
  )

  # Run basic checks
  testthat::expect_s3_class(object = meta, class = "data.frame")
  testthat::expect_true(nrow(meta) > 0)
})
