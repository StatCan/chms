#' @title Classify an accelerometer axis vector of 60-second epochs as sleep
#' time or awake time.
#' @description This function uses the Tudor-Locke algorithm
#' (https://pubmed.ncbi.nlm.nih.gov/24383507) to classify an accelerometer
#' axis vector of 60-second epochs as sleep time or awake time. This function
#' was validated indirectly against output from the official SAS version of the
#' Barreira algorithm (www.pbrc.edu/pdf/PBRCSleepEpisodeTimeMacroCode.pdf).
#' @param x Required: a data frame of accelerometer data in 60-second epochs.
#' @param time_stamp Required (default: `"dataTimestamp"`): a length-one
#' character vector representing the name of the time stamp vector.
#' @param axis1 Required (default: `"axis1"`): a length-one character vector
#' representing the name of the vertical axis.
#' @param incline_off Required (default: `"inclineOff"`): a length-one
#' character vector representing the name of the corresponding vector in an
#' ActiGraph `.agd` file.
#' @param incline_standing Required (default: `"inclineStanding"`): a
#' length-one character vector representing the name of the corresponding
#' vector in an ActiGraph `.agd` file.
#' @param incline_sitting Required (default: `"inclineSitting"`): a length-one
#' character vector representing the name of the corresponding vector in an
#' ActiGraph `.agd` file.
#' @param incline_lying Required (default: `"inclineLying"`): a length-one
#' character vector representing the name of the corresponding vector in an
#' ActiGraph `.agd` file.
#' @param return Required (default: `"everything"`): a character vector
#' representing which vectors to return. If set to "everything", the data frame
#' in `x` will be returned along with all vectors that were derived while
#' applying the Tudor-Locke algorithm.
#' @return Returns `x` along with all vectors that were derived while applying
#' the Tudor-Locke algorithm.
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
#' # Load and clean data
#' agd_data$load()$clean()
#'
#' # Apply Tudor-Locke sleep algorithm
#' dt <- apply_tudor_locke_algo(
#'   x = agd_data$data$clean |>
#'     dplyr::filter(
#'       epoch_length == 60,
#'       filter == "LowFrequencyExtension"
#'     )
#' )
#' @export

apply_tudor_locke_algo <- function(
  x,
  time_stamp = "dataTimestamp",
  axis1 = "axis1",
  incline_off = "inclineOff",
  incline_standing = "inclineStanding",
  incline_sitting = "inclineSitting",
  incline_lying = "inclineLying",
  return = "everything"
) {
  # Rename vectors
  x <- x |>
    dplyr::rename(
      time_stamp = dplyr::all_of(time_stamp),
      axis1 = dplyr::all_of(axis1),
      incline_off = dplyr::all_of(incline_off),
      incline_standing = dplyr::all_of(incline_standing),
      incline_sitting = dplyr::all_of(incline_sitting),
      incline_lying = dplyr::all_of(incline_lying)
    )

  # Apply Sadeh algorithm
  x <- x |>
    apply_sadeh_algo()

  # Apply inclinometer algorithm
  x <- x |>
    dplyr::mutate(
      # Compute inclinometer classification per Barreira
      inclinometer = dplyr::case_when(
        incline_lying == pmax(
          incline_off, incline_standing, incline_sitting, incline_lying
        ) ~ 2,
        incline_sitting == pmax(
          incline_off, incline_standing, incline_sitting, incline_lying
        ) ~ 3,
        incline_standing == pmax(
          incline_off, incline_standing, incline_sitting, incline_lying
        ) ~ 1,
        incline_off == pmax(
          incline_off, incline_standing, incline_sitting, incline_lying
        ) ~ 0,
        TRUE ~ NA
      ),

      # Adjust Sadeh sleep score per Barreira
      sadeh_sleep_score = dplyr::case_when(
        is.na(sadeh_sleep_score) ~ 1,
        TRUE ~ sadeh_sleep_score
      ),

      # Interpret Sadeh sleep score again per Barreira
      is_sleeping = ifelse(
        test = sadeh_sleep_score < 0 & inclinometer != 0,
        yes = "No",
        no = "Yes"
      )
    )

  # Add day, hour and noon vectors
  x <- x |>
    dplyr::mutate(
      day = format(time_stamp, format = "%Y-%m-%d") |>
        factor() |>
        as.integer(),
      hour = lubridate::hour(time_stamp),
      noon_day = dplyr::case_when(
        hour < 12 ~ (day - 1),
        TRUE ~ day
      )
    )

  # Classify sleep time by noon day (12:00pm to 11:59am)
  x <- dplyr::bind_rows(
    lapply(
      X = unique(x$noon_day),
      FUN = function(y) {

        # Filter on noon_day
        x2 <- x |> dplyr::filter(noon_day == y)

        # Add x vector (Sadeh sleep score) to a data frame
        df <- data.frame(sleep_score = x$is_sleeping)

        # Get run lengths (round 1)
        run_lengths <- rle(df$sleep_score)

        # Interpret sleep time run lengths
        run_lengths$values <- ifelse(
          test = run_lengths$values == "Yes" & run_lengths$lengths >= 5,
          yes = "Sleeping",
          no = NA
        )

        # Add to new vector
        df$is_sleeping <- inverse.rle(run_lengths)

        # Get run lengths (round 2)
        run_lengths <- rle(df$sleep_score)

        # Interpret awake time run lengths
        run_lengths$values <- ifelse(
          test = run_lengths$values == 0 & run_lengths$lengths >= 10,
          yes = "Awake",
          no = NA
        )

        # Add to new vector
        df$is_awake <- inverse.rle(run_lengths)

        # Impute missing values
        df$impute <- ifelse(
          test = ! is.na(df$is_sleeping),
          yes = df$is_sleeping,
          no = df$is_awake
        )

        # Continue to impute by carrying last observation forward
        df$impute <- zoo::na.locf(object = df$impute, na.rm = FALSE)

        # Get run lengths (round 3)
        run_lengths <- rle(df$impute)

        # Interpret sleep as sleep only if run lengths are at least 160 minutes
        run_lengths$values <- ifelse(
          test = run_lengths$values == "Sleeping" & run_lengths$lengths >= 160,
          yes = "Sleeping",
          no = "Awake"
        )

        # Add to new vector
        is_sleeping <- inverse.rle(run_lengths)

        # Recode values
        is_sleeping <- ifelse(
          test = is_sleeping == "Sleeping",
          yes = "Yes",
          no = "No"
        )

        # Exit
        x2
      }
    )
  )

  # Restore original vector names where changed
  x <- x |>
    dplyr::rename(
      !! time_stamp := time_stamp,
      !! axis1 := axis1,
      !! incline_off := incline_off,
      !! incline_standing := incline_standing,
      !! incline_sitting := incline_sitting,
      !! incline_lying := incline_lying
    )

  # Determine what to return and exit
  if ("everything" %in% return) x else x[return]
}
