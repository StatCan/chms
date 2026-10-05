#' @title Summary method for the [agd_worker] R6 class.
#' @description This method renders summary tibbles for waking and sleeping
#' hours from an [agd_worker] object to the console.
#' @param object Required: an [agd_worker] object.
#' @param ... Optional: additional arguments affecting the summary produced.
#' **Note:** currently not used.
#' @return Returns `NULL` invisibly.
#' @method summary agd_worker
#' @examples
#' # Initialize agd_worker R6 class
#' agd_data <- agd_worker$new(
#'   id = "jane-canuck",
#'   age = 10,
#'   agd_lfe = system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
#'   agd_nml = system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
#'   epoch_length = 15,
#'   day_max = 3,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira"
#' )
#'
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' agd_data$run()
#'
#' # Summarize data
#' summary(agd_data)
#' @export

summary.agd_worker <- function(object, ...) {
  # Render messages to console
  cli::cli_h2(
    paste0(
      cli::make_ansi_style("#af3c43")("\U1F341"),
      cli::make_ansi_style("#000000")(
        paste("{.emph chms::summary(agd_worker)} method")
      )
    )
  )
  cli::cli_text("{.strong Waking hours summary}")
  cli::cli_text(
    paste0(
      "Participant id: ",
      "{unique(object$results$summary_waking_hours$participant_id)}, ",
      "device: ",
      "{unique(object$results$summary_waking_hours$device_serial_number)}"
    )
  )
  cli::cli_alert_info(
    "Note: the last row contains averages and counts from valid days only"
  )
  cli::cli_text("")

  # Print waking hours stats
  object$results$summary_waking_hours |>
    dplyr::select(
      -summary,
      -participant_id,
      -device_serial_number,
      -participant_age,
      -steps_predicted,
      -steps_lfe
    ) |>
    dplyr::relocate(
      lpa,
      mpa,
      vpa,
      mvpa,
      lmvpa,
      mpa_bouts,
      vpa_bouts,
      mvpa_bouts,
      sb,
      .after = steps
    ) |>
    print()

  # Render message to console
  cli::cli_text("")
  cli::cli_text("{.strong Sleeping hours summary}")
  cli::cli_text(
    paste0(
      "Participant id: ",
      "{unique(object$results$summary_sleeping_hours$participant_id)}, ",
      "device: ",
      "{unique(object$results$summary_sleeping_hours$device_serial_number)}"
    )
  )
  cli::cli_alert_info(
    "Note: the last row contains averages and counts from valid days only"
  )
  cli::cli_text("")

  # Print sleeping hours stats
  object$results$summary_sleeping_hours |>
    dplyr::select(
      -summary,
      -participant_id,
      -device_serial_number,
      -participant_age
    ) |>
    dplyr::relocate(sleep_period_time, sleep_episodes, .after = wear_time) |>
    print()

  # Exit
  invisible(NULL)
}
