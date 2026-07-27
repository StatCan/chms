# Load participant meta
meta <- chms::get_chms_meta(
  clinic_file = config::get("cycle7_clinic_file"),
  agd_dir = config::get("cycle7_agd_dir")
) |>
  dplyr::arrange(site, id)

# Set progress bar
cli::cli_progress_bar(
  name = "Processing",
  total = nrow(meta)
)

# Iterate meta
for(i in 1:nrow(meta)) {
  # If agd_lfe file exists
  if(file.exists(meta$agd_lfe[i])) {
    # Load .agd data
    agd_data <- tryCatch(
      expr = {
        chms::load_agd_data(
          file = meta$agd_lfe[i],
          day_max = 100,
          start_date = NA
        )
      },
      error = function(e) NULL
    )

    # If successful data load
    if(! is.null(agd_data)) {
      # Set file name
      file_name <- paste0(meta$id[i], "LFE_", unique(agd_data$epoch_length), "sec.csv")

      # Select wanted columns
      agd_data <- agd_data |>
        dplyr::select(dataTimestamp:inclineLying)

      # Write to file locally
      readr::write_csv(
        x = agd_data,
        file = paste0(getwd(), "/", file_name),
        na = ""
      )

      # Move file to remote location
      file.rename(
        from = paste0(getwd(), "/", file_name),
        to = paste0(dirname(meta$agd_lfe[i]), "/", file_name)
      )
    }
  }

  # Update progress bar
  cli::cli_progress_update()
}

# Terminate progress bar
cli::cli_progress_done()
