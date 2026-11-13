# Load dependencies
library(chms)
library(dplyr)
library(stringr)

# Get participant meta
meta <- get_chms_meta(
  clinic_file = config::get("cycle7_clinic_file"),
  agd_dir = config::get("cycle7_agd_dir")
) |>
  arrange(site, id)

# Iterate sites 2-18
for (i in 2:18) {
  # Get site meta
  site_meta <- meta |>
    filter(site == i)

  # Set site-specific location for storing results
  site_dir <- paste0(
    config::get("cycle7_agd_dir"),
    str_pad(
      string = i,
      width = 2,
      pad = 0
    ),
    "/R processing"
  )

  # Initialize agd class
  agd_data <- agd$new(
    id = site_meta$id,
    age = site_meta$age,
    agd_lfe = site_meta$agd_lfe,
    agd_nml = site_meta$agd_nml,
    epoch_length = site_meta$epoch_length,
    day_max = 7,
    sleep_algo = "barreira",
    non_wear_algo = "barreira",
    start_date = site_meta$start_date,
    cpu_max = 15,
    dir = site_dir
  )

  # Process site data
  agd_data$run()

  # Save select results to "R processing" root
  write.csv(
    x = agd_data$results$summary_full,
    file = paste0(site_dir, "/output.csv"),
    row.names = FALSE,
    na = ""
  )

  write.csv(
    x = agd_data$results$summary_full_stc,
    file = paste0(site_dir, "/output-sspe-format.csv"),
    row.names = FALSE,
    na = ""
  )

  write.csv(
    x = agd_data$results$summary_run,
    file = paste0(site_dir, "/log.csv"),
    row.names = FALSE,
    na = ""
  )
}
