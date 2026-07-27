#' @title Load the `settings` table from an ActiGraph `.agd` file.
#' @description This function loads the `settings` table from an ActiGraph `.agd` file.
#' @param file Required: a length-one character vector representing the full path to an `.agd` file.
#' @return Returns a tibble of the `settings` table in a wide shape.
#' @export

load_agd_settings <- function(file) {
  # Connect to database
  connection <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = file
  )

  # Get settings table
  settings <- connection |>
    dplyr::tbl(dplyr::sql("SELECT * FROM settings")) |>
    dplyr::collect(n = Inf) |>
    dplyr::distinct(
      settingName,
      settingValue
    ) |>
    tidyr::pivot_wider(
      names_from = "settingName",
      values_from = "settingValue"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = startdatetime:downloaddatetime,
        .fns = ~ (as.numeric(.x) / 1e7) |>
          as.POSIXct(origin = "0001-01-01 00:00:00", tz = "UTC")
      ),
      time_zone = suppressWarnings(lubridate::tz(startdatetime)),
      dplyr::across(
        .cols = startdatetime:downloaddatetime,
        .fns = ~ format(
          x = .x,
          format = "%Y-%m-%d %H:%M:%S %Z"
        )
      )
    )

  # Close connection to database
  DBI::dbDisconnect(conn = connection)

  # Exit
  return(settings)
}
