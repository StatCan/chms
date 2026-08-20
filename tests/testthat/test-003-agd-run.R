test_that("ActiGraph data successfully processes", {
  # Skip if configs not found
  testthat::skip_on_cran()
  testthat::skip_if_not(nzchar(config::get("cycle7_agd_dir")))
  testthat::skip_if_not(nzchar(config::get("cycle7_clinic_file")))

  # Load participant meta and randomly select 50 participants
  meta <- get_chms_meta(
    clinic_file = config::get("cycle7_clinic_file"),
    agd_dir = config::get("cycle7_agd_dir")
  ) |>
    dplyr::slice_sample(n = 50)

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
    cpu_max = 15
  )

  # Run processing pipeline (load, clean, classify and summarize data)
  agd_data$run()

  # Run basic checks
  testthat::expect_true(class(agd_data$results) == "list")
  testthat::expect_true(length(agd_data$results) == 5)
  testthat::expect_true(nrow(agd_data$results$summary_full) > 0)
  testthat::expect_true(nrow(agd_data$results$summary_full_stc) > 0)
  testthat::expect_true(nrow(agd_data$results$summary_run) > 0)
  testthat::expect_true(nrow(agd_data$results$summary_sleeping_hours) > 0)
  testthat::expect_true(nrow(agd_data$results$summary_waking_hours) > 0)
})
