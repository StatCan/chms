#' @title Prepare an ActiGraph `.agd` file for downstream classification.
#' @description This function prepares an ActiGraph `.agd` file for downstream classification (e.g., remove incomplete days, aggregate data to 60-second epochs).
#' @param x Required: an [agd_worker] object.
#' @return Returns a `NULL` invisibly.
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
#' # Load data
#' agd_data$load()
#'
#' # Clean data
#' clean_agd_data(agd_data)
#'
#' # Store updated data
#' dt <- agd_data$data$clean
#' @export

clean_agd_data <- function(x) {
  # Create clean data frame
  x$data$clean <- x$data$raw |>
    dplyr::mutate(
      ymd_hm = format(
        x = dataTimestamp,
        format = "%Y-%m-%d %H:%M"
      ) |>
        as.POSIXct(tz = x$data$settings$time_zone)
    ) |>
    dplyr::relocate(ymd_hm, .before = dataTimestamp)

  # Detect any incomplete days
  incomplete_days <- x$data$clean |>
    dplyr::group_by(ymd) |>
    dplyr::reframe(
      status = ifelse(
        test = dplyr::n() == 24 * 60 * 60 / as.integer(x$data$settings$epochlength),
        yes = "complete",
        no = "incomplete"
      )
    ) |>
    dplyr::filter(status == "incomplete")

  # If any incomplete days found
  if(nrow(incomplete_days)) {
    # Remove
    x$data$clean <- x$data$clean |>
      dplyr::filter(! ymd %in% incomplete_days$ymd)
  }

  # If no data
  if(! nrow(x$data$clean)) {
    # Update issues
    x$issues$no_complete_days <- "yes"

    # Exit
    return(invisible(NULL))
  }

  # If data not already aggregated to 60-second epochs
  if(! 60 %in% unique(x$data$clean$epoch_length)) {
    # Aggregate and bind to x$data$clean
    x$data$clean <- dplyr::bind_rows(
      x$data$clean,
      x$data$clean |>
        dplyr::group_by(filter, epoch_length, ymd_hm) |>
        dplyr::reframe(
          dplyr::across(
            .cols = c(
              "axis1",
              "axis2",
              "axis3",
              "steps_normal",
              "steps_lfe",
              "inclineOff",
              "inclineStanding",
              "inclineSitting",
              "inclineLying"
            ),
            .fns = ~ sum(.x)
          ),
          dplyr::across(
            .cols = c(
              "lux"
            ),
            .fns = ~ floor(mean(.x))
          ),
          dplyr::across(
            .cols = c(
              "midnight_day",
              "ymd",
              "dataTimestamp"
            ),
            .fns = ~ dplyr::first(.x)
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(epoch_length = 60) |>
        dplyr::relocate(midnight_day, ymd, ymd_hm, dataTimestamp, .before = axis1) |>
        dplyr::relocate(lux, .after = steps_lfe)
    )
  }

  # Exit
  return(invisible(NULL))
}
