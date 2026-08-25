#' @title Plot method for the [agd] R6 class.
#' @description This method renders scatter plots iteratively and interactively using results from an [agd] object.
#' @param x Required: an [agd] object.
#' @param ... Optional: arguments to be passed to methods. **Note:** currently not used.
#' @param id Optional: a vector representing participant IDs in `x` for which to render scatter plots. If `id` is unset, scatter plots for all participants in `x` will be rendered iteratively.
#' @return Returns `ggplot2` objects invisibly.
#' @method plot agd
#' @examples
#' # Initialize agd R6 class
#' agd_data <- agd$new(
#'   id = "jane-canuck",
#'   age = 10,
#'   agd_lfe = system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
#'   agd_nml = system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
#'   epoch_length = 15,
#'   day_max = 2,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira"
#' )
#'
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' agd_data$run()
#'
#' # Plot data
#' plot(agd_data)
#' @export

plot.agd <- function(x, ..., id) {
  # Render messages to console
  cli::cli_h2(paste0(cli::make_ansi_style("#af3c43")("\U1F341"), cli::make_ansi_style("#000000")("{.emph chms::plot(agd)} method")))

  # Throw error if suggested packages are not installed but needed
  if(! rlang::is_installed("ggplot2")) {
    cli::cli_abort("Please install the {.pkg ggplot2} package.")
  } else if(! rlang::is_installed("scales")) {
    cli::cli_abort("Please install the {.pkg scales} package.")
  # Throw error if no data available
  } else if(sum(x$results$summary_run$days_loaded > 0) < 1) {
    cli::cli_abort("There is no data available to plot.")
  }

  # Get valid jobs
  jobs <- dplyr::right_join(
    x = x$jobs,
    y = x$results$summary_run |>
      dplyr::filter(days_loaded > 0) |>
      dplyr::select(participant_id) |>
      dplyr::rename(id = participant_id),
    by = "id"
  )

  # If id set, filter jobs
  if(! missing(id)) jobs <- jobs |> dplyr::filter(id %in% !! id)

  # Throw error if no jobs
  if(! nrow(jobs)) {
    cli::cli_abort("There is no data available to plot.")
  }

  # Iterate jobs
  for(i in 1:nrow(jobs)) {
    # Render message to console
    if(i > 1) cli::cli_text("")
    cli::cli_alert_info("Rendering scatter plot for participant {.var {jobs$id[i]}}")

    # Initialize agd_worker and run pipeline
    agd_data <- agd_worker$new(
      id = jobs$id[i],
      age = jobs$age[i],
      agd_lfe = jobs$agd_lfe[i],
      agd_nml = jobs$agd_nml[i],
      epoch_length = jobs$epoch_length[i],
      day_max = jobs$day_max[i],
      sleep_algo = jobs$sleep_algo[i],
      non_wear_algo = jobs$non_wear_algo[i],
      start_date = jobs$start_date[i]
    )$load()$clean()$classify()

    # Get plot.agd_worker
    plot <- plot(agd_data)

    # Render plot
    print(plot)

    # Render message to console
    cli::cli_text("")
    cli::cli_alert_success("Done!")

    # If not on the last job
    if(i < nrow(jobs)) {
      # Prompt user
      cli::cli_text("")
      user_response <- readline(prompt = 'Press the [Enter] button to render the next plot or press the [q] button to quit: ')

      # Break loop if user wants to quit
      if(tolower(user_response) == "q") break
    }
  }

  # Exit
  return(invisible(plot))
}
