testthat::test_that("Settings table loads successfully from sample .agd file", {
  # Skip if config not found
  testthat::skip_if_not(nzchar(config::get("agd_file")))

  # Load settings table
  settings <- load_agd_settings(config::get("agd_file"))

  # Run basic checks
  testthat::expect_s3_class(object = settings, class = "data.frame")
  testthat::expect_true(nrow(settings) > 0)
})
