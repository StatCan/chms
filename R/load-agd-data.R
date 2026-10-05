#' @title Load the `data` table from an ActiGraph `.agd` file.
#' @description This function loads the `data` table from an ActiGraph `.agd`
#' file.
#' @param file Required: a length-one character vector representing the full
#' path to an `.agd` file.
#' @param col_select Required (default: "`everything`"): a character vector
#' representing which vectors to load.
#' @param day_max Required (default: `7`): a length-one integer vector
#' representing the maximum number of days of data to load from `file`.
#' @param start_date Optional: a length-one date vector (format: yyyy-mm-dd)
#' representing the first day of data to load from `file`. If not set, data
#' will be loaded from the first available day until `day_max` is reached.
#' @param settings Optional: a tibble from `file` previously returned from
#' [load_agd_settings()].
#' @return Returns a tibble of the `data` table.
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
#' # Load data with low frequency extension
#' dt <- load_agd_data(
#'   file = agd_data$args$agd_lfe,
#'   start_date = agd_data$args$start_date
#' )
#' @export

load_agd_data <- function(
  file,
  col_select = "everything",
  day_max = 7,
  start_date,
  settings
) {
  # Connect to database
  connection <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = file
  )

  # Close connection to database on exit
  on.exit(expr = DBI::dbDisconnect(conn = connection), add = TRUE)

  # Load settings
  if (missing(settings)) settings <- load_agd_settings(file)

  # Set start and stop dates
  if (missing(start_date) || is.na(start_date)) {
    start_date <- suppressWarnings(lubridate::date(settings$startdatetime))
  }
  stop_date <- start_date + day_max - 1

  # Get available dates
  dates <- data.frame(
    ymd = seq(
      from = start_date,
      to = stop_date,
      by = "day"
    )
  ) |>
    dplyr::mutate(midnight_day = seq_len(dplyr::n())) |>
    dplyr::filter(midnight_day <= day_max) |>
    dplyr::mutate(
      ymd2 = dplyr::case_when(
        dplyr::row_number() == dplyr::n() ~
          as.POSIXct(x = paste0(ymd, " 23:59:59"), tz = settings$time_zone),
        TRUE ~ as.POSIXct(x = paste0(ymd, " 00:00:00"), tz = settings$time_zone)
      ),
      seconds_since_1970 = as.POSIXct(
        x = ymd2,
        tz = settings$time_zone
      ) |>
        as.numeric(),
      offset = as.POSIXct(
        x = "1970-01-01 00:00:00",
        tz = settings$time_zone
      ) |>
        as.numeric() -
        as.POSIXct(
          x = "0001-01-01 00:00:00",
          tz = settings$time_zone
        ) |>
          as.numeric(),
      seconds_since_0001 = seconds_since_1970 + offset,
      windows_tick = seconds_since_0001 * 1e7
    ) |>
    dplyr::relocate(midnight_day)

  # Get data table
  data <- DBI::dbGetQuery(
    conn = connection,
    statement = sprintf(
      fmt = "SELECT %s FROM data WHERE dataTimestamp BETWEEN %f AND %f",
      paste(
        ifelse(
          test = col_select == "everything",
          yes = "*",
          no = col_select
        ),
        collapse = ","
      ),
      dates$windows_tick[1],
      dates$windows_tick[nrow(dates)]
    )
  ) |>
    dplyr::mutate(
      dataTimestamp = (dataTimestamp / 1e7) |>
        as.POSIXct(origin = "0001-01-01 00:00:00", tz = settings$time_zone),
      ymd = as.Date(x = dataTimestamp, format = "%Y-%m-%d"),
      epoch_length = as.integer(settings$epochlength),
      filter = settings$filter
    ) |>
    dplyr::left_join(
      y = dates |>
        dplyr::select(ymd, midnight_day),
      by = "ymd"
    ) |>
    dplyr::relocate(filter, epoch_length, midnight_day, ymd)

  # Exit
  data
}
