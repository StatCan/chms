#' @title Summary method for the [agd] R6 class.
#' @description This method renders summary tibbles for waking and sleeping hours from an [agd] object to the console.
#' @param object Required: an [agd_worker] object.
#' @param ... Optional: additional arguments affecting the summary produced. **Note:** currently not used.
#' @param row_max Required (default: `10`): a length-one integer vector representing the number of rows of summary data to render to the console.
#' @return Returns `NULL` invisibly.
#' @method summary agd
#' @examples \donttest{
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
#'   day_max = 7,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira",
#'   start_date = meta$start_date,
#'   cpu_max = 2
#' )
#'
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' agd_data$run()
#'
#' # Summarize data
#' summary(agd_data)
#' }
#' @export

summary.agd <- function(object, ..., row_max = 10) {
  # Render messages to console
  cli::cli_h2(paste0(cli::make_ansi_style("#af3c43")("\U1F341"), cli::make_ansi_style("#000000")("{.emph chms::summary(agd)} method")))
  cli::cli_text("{.strong Waking hours summary}")
  cli::cli_text(
    paste0(
      "Participant count: {length(unique(object$results$summary_waking_hours$participant_id))}"
    )
  )
  cli::cli_text("")

  # Print waking hours stats
  object$results$summary_waking_hours |>
    dplyr::filter(stringr::str_detect(string = summary, pattern = "average")) |>
    dplyr::select(-summary, -day, -day_of_week, -participant_age, -steps_predicted, -steps_lfe) |>
    dplyr::relocate(lpa, mpa, vpa, mvpa, lmvpa, mpa_bouts, vpa_bouts, mvpa_bouts, sb, .after = steps) |>
    print(n = row_max)

  # Render message to console
  cli::cli_text("")
  cli::cli_text("{.strong Sleeping hours summary}")
  cli::cli_text(
    paste0(
      "Participant count: {length(unique(object$results$summary_sleeping_hours$participant_id))}"
    )
  )
  cli::cli_text("")

  # Print sleeping hours stats
  object$results$summary_sleeping_hours |>
    dplyr::filter(stringr::str_detect(string = summary, pattern = "average")) |>
    dplyr::select(-summary, -participant_age, -day, -day_of_week, -nocturnal_sleep_onset, -nocturnal_sleep_offset) |>
    dplyr::relocate(sleep_period_time, sleep_episodes, .after = wear_time) |>
    print(n = row_max)

  # Exit
  return(invisible(NULL))
}
