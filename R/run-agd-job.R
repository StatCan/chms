#' @title Run a job that initiates an ActiGraph data processing pipeline for a single participant.
#' @description This function runs a job that processes ActiGraph data for a single participant by calling `agd_worker$new()$run()`. This function is used heavily by the [agd] R6 class.
#' @param x Required: a one-row tibble from an [agd] object `jobs` data structure.
#' @return Returns the `results` list from an [agd_worker] object.
#' @examples
#' # Create meta data frame (external/non-statcan users)
#' meta <- data.frame(
#'   id = c("jane-canuck", "john-canuck"),
#'   age = c(10, 40),
#'   agd_lfe = c(
#'     system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
#'     system.file("extdata", "john-canuck-lfe.agd", package = "chms")
#'   ),
#'   agd_nml = c(
#'     system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
#'     system.file("extdata", "john-canuck-nml.agd", package = "chms")
#'   ),
#'   start_date = c("2021-05-30", "2021-05-27"),
#'   epoch_length = c(15, 60)
#' )
#'
#' # Initialize agd R6 class
#' agd_data <- agd$new(
#'   id = meta$id,
#'   age = meta$age,
#'   agd_lfe = meta$agd_lfe,
#'   agd_nml = meta$agd_nml,
#'   epoch_length = meta$epoch_length,
#'   day_max = 3,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira",
#'   start_date = meta$start_date,
#'   cpu_max = 2
#' )
#'
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' # on first participant in agd_data
#' lst <- run_agd_job(agd_data$jobs[1,])
#' @export

run_agd_job <- function(x) {
  # Initialize and run agd_worker class
  obj <- agd_worker$new(
    id = x$id,
    age = x$age,
    agd_nml = x$agd_nml,
    agd_lfe = x$agd_lfe,
    day_max = x$day_max,
    start_date = x$start_date,
    sleep_algo = x$sleep_algo,
    non_wear_algo = x$non_wear_algo,
    epoch_length = x$epoch_length
  )$run()

  # Add run results to obj$results
  obj$results$summary_run <- obj$log |>
    dplyr::mutate(
      participant_id = x$id,
      run_time = dplyr::case_when(
        dplyr::row_number() == dplyr::n() ~ as.numeric(difftime(max(timestamp), min(timestamp))),
        TRUE ~ NA
      )
    ) |>
    dplyr::relocate(participant_id)

  # Flatten run into single row
  obj$results$summary_run <- obj$results$summary_run |>
    dplyr::group_by(participant_id) |>
    dplyr::reframe(
      run_time = max(run_time, na.rm = TRUE),
      device = obj$data$settings$deviceserial,
      device_start = obj$data$settings$startdatetime,
      epoch_length = obj$data$settings$epochlength,
      day_max = obj$args$day_max,
      days_loaded = ifelse(
        test = "classify" %in% names(obj$data),
        yes = max(obj$data$classify$midnight_day, na.rm = TRUE),
        no = 0
      ),
      file_missing = obj$issues$file_missing,
      file_empty = obj$issues$file_empty,
      files_identical = obj$issues$files_identical,
      files_mismatched = obj$issues$files_mismatched,
      non_midnight_start = obj$issues$non_midnight_start,
      no_complete_days = obj$issues$no_complete_days,
      sleep_missing = obj$issues$sleep_missing,
      age_out_of_range = obj$issues$age_out_of_range,
      sleep_algo = obj$args$sleep_algo,
      sleep_epoch_length = obj$args$sleep_epoch_length,
      non_wear_algo = obj$args$non_wear_algo,
      non_wear_epoch_length = obj$args$non_wear_epoch_length,
      movement_epoch_length = obj$args$movement_epoch_length,
      sb_cutpoints = obj$args$sb_cutpoints,
      lpa_cutpoints = obj$args$lpa_cutpoints,
      mpa_cutpoints = obj$args$mpa_cutpoints,
      vpa_cutpoints = obj$args$vpa_cutpoints,
      device_settings = ifelse(
        test = ! "settings" %in% names(obj$data) | "error" %in% class(obj$data$settings),
        yes = jsonlite::toJSON(dplyr::tibble()),
        no = jsonlite::toJSON(obj$data$settings)
      ),
      args = jsonlite::toJSON(obj$args),
      issues = jsonlite::toJSON(obj$issues),
      log = jsonlite::toJSON(obj$results$summary_run)
    )

  # Sort results tibbles by name
  obj$results <- obj$results[sort(names(obj$results))]

  # Exit
  return(obj$results)
}
