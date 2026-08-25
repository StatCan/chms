# Install chms
remotes::install_git(
  url = "https://github.com/statcan/chms",
  force = TRUE,
  upgrade = "never"
)

# Load dependencies into current R session
library(chms)
library(dplyr)

# Create participant meta (external/non-statcan users)
meta <- tibble(
  id = c("jane-canuck", "john-canuck"),
  age = c(10, 40),
  agd_lfe = c(
    system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
    system.file("extdata", "john-canuck-lfe.agd", package = "chms")
  ),
  agd_nml = c(
    system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
    system.file("extdata", "john-canuck-nml.agd", package = "chms")
  ),
  start_date = c("2021-05-30", "2021-05-27"),
  epoch_length = c(15, 60)
)

# Print/examine
glimpse(meta)

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
  cpu_max = 2
)

# Print/examine
agd_data

# Run processing pipeline (load, clean, classify and summarize data)
agd_data$run()

# Export results
agd_data$export(dir = tempdir())

# Export statcan-formatted results
agd_data$export(dir = tempdir(), stc = TRUE)

# Get settings and pipeline run log
agd_data

# Plot data
plot(agd_data, id = "jane-canuck")

# Summarize data
summary(agd_data)

# View all results in tab
agd_data$view()

# View specific results in tab
agd_data$view("summary_full")
agd_data$view("summary_full_stc")
agd_data$view("summary_run")
agd_data$view("summary_sleeping_hours")
agd_data$view("summary_waking_hours")

# View issues and run log
agd_data$view("issues")
agd_data$view("log")

# Store results in stand-alone data frames
summary_full <- agd_data$results$summary_full
summary_full_stc <- agd_data$results$summary_full_stc
summary_run <- agd_data$results$summary_run
summary_sleeping_hours <- agd_data$results$summary_sleeping_hours
summary_waking_hours <- agd_data$results$summary_waking_hours

# Render sanity check report
agd_data$sanity_check(dir = tempdir(), name = "My sanity check report")
