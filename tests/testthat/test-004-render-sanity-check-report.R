test_that("Sanity check report in Quarto successfully renders", {
  # Skip if configs not found
  testthat::skip_if_not(nzchar(config::get("cycle7_agd_dir")))
  testthat::skip_if_not(nzchar(config::get("cycle7_clinic_file")))

  # Load participant meta and randomly select 10 participants
  meta <- get_chms_meta(
    clinic_file = config::get("cycle7_clinic_file"),
    agd_dir = config::get("cycle7_agd_dir")
  ) |>
    dplyr::slice_sample(n = 10)

  # Initialize agd R6 class
  agd_data <- agd$new(
    id = meta$id,
    age = meta$age,
    agd_lfe = meta$agd_lfe,
    agd_nml = meta$agd_nml,
    epoch_length = meta$epoch_length,
    day_max = 7,
    sleep_algo = "barreira",
    non_wear_algo = "barreira",
    start_date = meta$start_date,
    cpu_max = 10,
    dir = getwd()
  )

  # Run processing pipeline (load, clean, classify and summarize data)
  agd_data$run()

  # Run basic checks
  testthat::expect_no_error(
    agd_data$sanity_check(
      name = "My sanity check report",
      dir = getwd()
    )
  )
  testthat::expect_true(file.exists(paste0(getwd(), "/My sanity check report.html")))
})
