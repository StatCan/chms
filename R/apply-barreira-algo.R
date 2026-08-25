#' @title Classify an accelerometer axis vector of 60-second epochs as sleep time or awake time.
#' @description This function uses the Barreira algorithm (pubmed.ncbi.nlm.nih.gov/25202840) to classify an accelerometer axis vector of 60-second epochs as sleep time or awake time. This function was validated against output from the official SAS version of the algorithm (www.pbrc.edu/pdf/PBRCSleepEpisodeTimeMacroCode.pdf).
#' @param x Required: a data frame of accelerometer data in 60-second epochs.
#' @param time_stamp Required (default "`dateTimestamp`"): a length-one character vector representing the name of the timestamp vector.
#' @param axis1 Required (default: "`axis1`"): a length-one character vector representing the name of the vertical axis.
#' @param incline_off Required (default: "`inclineOff`"): a length-one character vector representing the name of the corresponding vector in an ActiGraph .agd file.
#' @param incline_standing Required (default: "`inclineStanding`"): a length-one character vector representing the name of the corresponding vector in an ActiGraph .agd file.
#' @param incline_sitting Required (default: "`inclineSitting`"): a length-one character vector representing the name of the corresponding vector in an ActiGraph .agd file.
#' @param incline_lying Required (default: "`inclineLying`"): a length-one character vector representing the name of the corresponding vector in an ActiGraph .agd file.
#' @param age Required: a length-one integer vector representing the participant's age in years. This parameter is used to determine when the first sleep bout can begin. Statistics Canada sets the time to 18:00 for participants under five years and younger, and 19:00 for all other ages.
#' @param return Required (default: "`everything`"): a character vector representing which vectors in `x` to return. If set to "`everything`", the data frame in `x` will be returned along with all vectors that were derived while applying the Barreira algorithm.
#' @return Returns the data frame `x` along with all vectors that were derived while applying the Barreira algorithm.
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
#'
#' # Apply Barreira sleep algorithm
#' dt <- apply_barreira_algo(
#'   x = agd_data$data$clean |>
#'     dplyr::filter(
#'       epoch_length == 60,
#'       filter == "LowFrequencyExtension"
#'     ),
#'   age = agd_data$args$age
#' )
#' @export

apply_barreira_algo <- function(
    x,
    time_stamp = "dataTimestamp",
    axis1 = "axis1",
    incline_off = "inclineOff",
    incline_standing = "inclineStanding",
    incline_sitting = "inclineSitting",
    incline_lying = "inclineLying",
    age,
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
  x <- x |> apply_sadeh_algo(censor_counts = FALSE)

  # Apply inclinometer algorithm
  x <- x |>
    dplyr::mutate(
      # Compute inclinometer classification per Barreira
      inclinometer = dplyr::case_when(
        incline_lying == pmax(incline_off, incline_standing, incline_sitting, incline_lying) ~ 2,
        incline_sitting == pmax(incline_off, incline_standing, incline_sitting, incline_lying) ~ 3,
        incline_standing == pmax(incline_off, incline_standing, incline_sitting, incline_lying) ~ 1,
        incline_off == pmax(incline_off, incline_standing, incline_sitting, incline_lying) ~ 0,
        TRUE ~ NA
      ),

      # Adjust Sadeh sleep score per Barreira
      sadeh_sleep_score = dplyr::case_when(
        is.na(sadeh_sleep_score) ~ 1,
        TRUE ~ sadeh_sleep_score
      ),

      # Interpret Sadeh sleep score again per Barreira
      is_sleeping = ifelse(
        test = sadeh_sleep_score >= 0 | inclinometer == 0,
        yes = "Yes",
        no = "No"
      )
    )

  # Add day, hour and noon vectors
  x <- x |>
    dplyr::mutate(
      day = format(time_stamp, format = "%Y-%m-%d") |> factor() |> as.integer(),
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

        # Set sleep start time
        sleep_start_time <- 12

        # Filter on hours that are eligible for sleep (e.g., 7pm on day 1 until 11:59am on day 2)
        x3 <- x2 |> dplyr::filter(hour >= sleep_start_time | hour < 12)

        # Compute run lengths and values of equal values and reshape as data frame
        df <- x3$is_sleeping |>
          rle() |>
          inverse.rle() |>
          rle() |>
          "class<-"("list") |>
          as.data.frame() |>
          dplyr::rename(
            length = lengths,
            value = values
          ) |>
          dplyr::mutate(
            value = dplyr::case_when(
              value == "Yes" & length >= 5 ~ "Sleep 5+",
              value == "Yes" ~ "Sleep",
              value == "No" & length >= 21 ~ "Awake 21+",
              value == "No" & length >= 11 ~ "Awake 11+",
              value == "No" ~ "Awake",
              TRUE ~ NA
            ),
            hour = NA,
            hour_start = NA,
            hour_end = NA
          )

        # Add hour vector
        # Important later when evaluating whether a sleep bout has ended (e.g., only 11+ minutes of awake time needed after 5am)
        #for(i in 1:nrow(df)) df$hour[i] <- max(lubridate::hour(x3$time_stamp[sum(df$length[1:i])]))
        for(i in 1:nrow(df)) {

          length_sum_upper_end <- sum(df$length[1:i])
          length_sum_lower_end <- length_sum_upper_end - df$length[i] + 1
          df$hour[i] <- df$hour_end[i] <- lubridate::hour(max(c(x3$time_stamp[length_sum_lower_end], x3$time_stamp[length_sum_upper_end])))
          df$hour_start[i] <- lubridate::hour(min(c(x3$time_stamp[length_sum_lower_end], x3$time_stamp[length_sum_upper_end])))

        }

        # Get all row numbers for values of sleep (i.e. "Sleep", "Sleep 5+")
        sleep_row_numbers <- which(stringr::str_detect(df$value, "Sleep"))

        # Get all row numbers for values of sleep that start a sleep bout
        sleep_bout_start_rows <- which(df$value == "Sleep 5+")

        # Get all row numbers for values of awake time that can end a sleep bout
        sleep_bout_stop_rows <- which(df$value %in% c("Awake 11+", "Awake 21+"))

        # Create a grid search
        grid_search <- expand.grid(
          sleep_bout_start_row = sleep_bout_start_rows,
          sleep_bout_stop_row = sleep_bout_stop_rows
        ) |>
          dplyr::filter(sleep_bout_stop_row >= sleep_bout_start_row) |>
          dplyr::arrange(sleep_bout_start_row)

        # If no sleep_bout_stop_rows values exist
        if(nrow(grid_search) < 1 & "Sleep 5+" %in% df$value) {

          # Get age-appropriate sleep start time for a given day
          sleep_start_time <- dplyr::if_else(
            condition = age >= 6,
            true = 19,
            false = 18
          )

          # Compute bedtime start and stop times
          bedtime_start <- x3$time_stamp[sum(df$length[1:(min(which(df$value == "Sleep 5+")) - 1)])] + 60
          bedtime_stop <- x3$time_stamp[sum(df$length[1:max(which(stringr::str_detect(df$value, "Sleep")))])]

          # Look for early bedtime starts
          early_bedtime_start <- which(lubridate::hour(bedtime_start) >= 12 & lubridate::hour(bedtime_start) < sleep_start_time)

          if(length(early_bedtime_start)) {

            # Reset is_sleeping vector
            x2$is_sleeping <- "No"

          } else {

            # Add is_sleeping vector to x2
            x2 <- x2 |>
              dplyr::mutate(
                is_sleeping = dplyr::case_when(
                  time_stamp >= bedtime_start & time_stamp <= bedtime_stop ~ "Yes",
                  TRUE ~ "No"
                )
              )

            if(sum(x2$is_sleeping == "Yes") < 160) x2$is_sleeping <- "No"

          }

          # Else, if a grid exists to search
        } else if(nrow(grid_search) > 0) {

          # Set default variables for grid search
          skip_sleep_bout_start_row <- next_sleep_bout_start_row <- 0
          bedtime_start <- bedtime_stop <- c()

          # Iterate grid_search
          for(i in 1:nrow(grid_search)) {

            # Skip an iteration if applicable
            if(
              grid_search$sleep_bout_start_row[i] == skip_sleep_bout_start_row |
              grid_search$sleep_bout_start_row[i] < next_sleep_bout_start_row
            ) next

            # Compute cumulative sleep time between sleep_bout_start_row and sleep_bout_stop_row
            # Count sleep minutes and awake minutes except for the final run length (which ends the potential sleep bout)
            cumulative_sleep_time <- sum(df$length[grid_search$sleep_bout_start_row[i]:(grid_search$sleep_bout_stop_row[i] - 1)])

            # Evaluate conditions
            condition1 <- cumulative_sleep_time < 160 &
              df$value[grid_search$sleep_bout_stop_row[i]] %in% c("Awake 11+", "Awake 21+")

            condition2 <- cumulative_sleep_time >= 160 &
              df$hour_end[grid_search$sleep_bout_stop_row[i]] %in% c(0:4, sleep_start_time:23) &
              df$value[grid_search$sleep_bout_stop_row[i]] %in% c("Awake 21+")

            condition3 <- cumulative_sleep_time >= 160 & df$hour_end[grid_search$sleep_bout_stop_row[i]] %in% 5:11 &
              df$value[grid_search$sleep_bout_stop_row[i]] %in% c("Awake 11+", "Awake 21+")

            if(condition1) {

              skip_sleep_bout_start_row <- grid_search$sleep_bout_start_row[i]

            }  else if(condition2 | condition3) {

              bedtime_start <- append(
                x = bedtime_start,
                values = ifelse(
                  test = grid_search$sleep_bout_start_row[i] == 1,
                  yes = 1,
                  no = sum(df$length[1:max(1, (grid_search$sleep_bout_start_row[i] - 1))]) + 1
                )
              )

              bedtime_stop <- append(
                x = bedtime_stop,
                values = ifelse(
                  test = grid_search$sleep_bout_stop_row[i] == nrow(df) & ! stringr::str_detect(df$value[nrow(df)], "Awake"),
                  yes = sum(df$length[1:nrow(df)]),
                  no = sum(df$length[1:max(1, (grid_search$sleep_bout_stop_row[i] - 1))])
                )
              )

              # Search for any other sleep bout start rows
              more_sleep_bout_start_rows <- which(df$value == "Sleep 5+")[which(df$value == "Sleep 5+") > grid_search$sleep_bout_stop_row[i]]

              # If more rows exist, skip ahead; otherwise, end grid search
              if(length(more_sleep_bout_start_rows) >= 1) next_sleep_bout_start_row <- more_sleep_bout_start_rows[1] else break

            }

          }

          if(length(bedtime_start)) bedtime_start <- x3$time_stamp[bedtime_start]
          if(length(bedtime_stop)) bedtime_stop <- x3$time_stamp[bedtime_stop]

          # If sleep period(s) found
          if(lubridate::is.POSIXct(bedtime_start) & lubridate::is.POSIXct(bedtime_stop) & length(bedtime_start) == length(bedtime_stop)) {

            # Reset is_sleeping vector
            x2$is_sleeping <- "No"

            # Get age-appropriate sleep start time for a given day
            sleep_start_time <- dplyr::if_else(
              condition = age >= 6,
              true = 19,
              false = 18
            )

            # Look for early bedtime starts
            early_bedtime_start <- which(lubridate::hour(bedtime_start) >= 12 & lubridate::hour(bedtime_start) < sleep_start_time)

            # If early bedtime starts exist, remove those sleep bouts
            if(length(early_bedtime_start)) {

              bedtime_start <- bedtime_start[-c(early_bedtime_start)]
              bedtime_stop <- bedtime_stop[-c(early_bedtime_start)]

            }

            # Look for late bedtime starts (on or after 6am)
            late_bedtime_start <- which(lubridate::hour(bedtime_start) >= 6 & lubridate::hour(bedtime_start) <= 11)

            # If late bedtime starts exist, remove those sleep bouts
            if(length(late_bedtime_start)) {
              bedtime_start <- bedtime_start[-c(late_bedtime_start)]
              bedtime_stop <- bedtime_stop[-c(late_bedtime_start)]
            }

            if(length(bedtime_start)) {

              # If multiple sleep bouts exist
              if(length(bedtime_start) >= 2) {

                # Iterate sleep bouts
                for(i in 2:length(bedtime_start)) {

                  # Compute time difference between two sleep bouts
                  time_between_sleep_periods <- bedtime_start[i] - bedtime_stop[i - 1]

                  # Remove any sleep bouts on/after 6am that are separated from previous bout by 20+ minutes
                  if(lubridate::hour(bedtime_start[i]) >= 6 & lubridate::hour(bedtime_start[i]) <= 11 & time_between_sleep_periods >= 20 & attributes(time_between_sleep_periods)$units == "mins") {

                    bedtime_start <- bedtime_start[1:(i - 1)]
                    bedtime_stop <- bedtime_stop[1:(i - 1)]
                    break

                  }

                }

              }

              # Update is_sleeping vector
              for(i in 1:length(bedtime_start)) {
                x2 <- x2 |>
                  dplyr::mutate(
                    is_sleeping = dplyr::case_when(
                      time_stamp >= !! bedtime_start[i] & time_stamp <= !! bedtime_stop[i] ~ "Yes",
                      TRUE ~ is_sleeping
                    )
                  )
              }

              if(sum(x2$is_sleeping == "Yes") < 160) x2$is_sleeping <- "No"

            } else {

              # Reset is_sleeping vector
              x2$is_sleeping <- "No"

            }

          } else {

            # Reset is_sleeping vector
            x2$is_sleeping <- "No"

          }

        } else {

          # Reset is_sleeping vector
          x2$is_sleeping <- "No"

        }

        # Derive sleep bout variables
        x2 <- x2 |>
          dplyr::mutate(
            sleep_bout = cumsum(is_sleeping == "Yes" & ! dplyr::lag(is_sleeping == "Yes", default = FALSE)),
            sleep_bout = dplyr::case_when(
              sleep_bout > 0 & is_sleeping == "No" ~ 0,
              TRUE ~ sleep_bout
            ),
            between_sleep_bouts = dplyr::case_when(
              is_sleeping == "No" & cumsum(is_sleeping == "Yes") > 0 & rev(cumsum(rev(is_sleeping == "Yes")) > 0) ~ "Yes",
              TRUE ~ "No"
            ),
            awake_bout = cumsum(between_sleep_bouts == "Yes" & ! dplyr::lag(between_sleep_bouts == "Yes", default = FALSE)),
            awake_bout = dplyr::case_when(
              awake_bout > 0 & between_sleep_bouts == "No" ~ 0,
              TRUE ~ awake_bout
            ),
          )

        return(x2)

      }
    )
  )

  # Classify wear/non-wear time by day (12:00am to 11:59pm) during waking hours
  x <- dplyr::bind_rows(
    lapply(
      X = unique(x$day),
      FUN = function(y) {

        # Filter on day
        x2 <- x |> dplyr::filter(day == y)

        # Add is_wearing vector ("No" = 20+ consecutive axis1 counts >= 1 outside of the bedtime start/stop window)
        x2 <- x2 |>
          dplyr::mutate(
            is_wearing = x2 |>
              dplyr::mutate(
                is_wearing = dplyr::case_when(
                  is_sleeping == "Yes" ~ "Wear",
                  axis1 >= 1 ~ "Wear",
                  axis1 == 0 ~ "Non-wear",
                  TRUE ~ NA
                )
              ) |>
              dplyr::select(is_wearing) |>
              unlist() |>
              rle() |>
              "class<-"("list") |>
              as.data.frame() |>
              dplyr::mutate(
                values = dplyr::case_when(
                  lengths >= 20 & values == "Non-wear" ~ "No",
                  TRUE ~ "Yes"
                )
              ) |>
              inverse.rle()
          )

        return(x2)

      }
    )
  )

  # Classify wear/non-wear time by noon day (12:00pm to 11:59am) during previously classified sleeping hours
  x <- dplyr::bind_rows(
    lapply(
      X = unique(x$noon_day),
      FUN = function(y) {

        # Filter on noon day
        x2 <- x |> dplyr::filter(noon_day == y)

        # If a sleep period exists
        if(sum(x2$is_sleeping == "Yes") >= 1) {

          # Get sleep window (start of bout 1 to end of last bout)
          sleep_window <- x2 |>
            dplyr::filter(
              time_stamp >= min(x2$time_stamp[x2$is_sleeping == "Yes"]),
              time_stamp <= max(x2$time_stamp[x2$is_sleeping == "Yes"])
            ) |>
            dplyr::select(time_stamp, axis1)

          # Create is_wearing_during_sleep
          x2 <- x2 |>
            dplyr::mutate(
              is_wearing_during_sleep = dplyr::case_when(
                time_stamp %in% sleep_window$time_stamp & is_sleeping == "Yes" ~ "Yes",
                time_stamp %in% sleep_window$time_stamp & is_sleeping == "No" ~ "No",
                TRUE ~ NA
              )
            )

          # Set flags
          in_bout <- FALSE
          target <- 0
          exceptions <- 0
          max_exceptions <- 2
          df <- c()

          # Iterate sleep_window
          for(i in 1:nrow(sleep_window)) {
            if(! in_bout) {
              in_bout <- TRUE
              df <- dplyr::bind_rows(
                df,
                data.frame(
                  bout_start = i,
                  bout_start_time_stamp = sleep_window$time_stamp[i],
                  bout_stop = NA,
                  bout_stop_time_stamp = sleep_window$time_stamp[i]
                )
              )
            }

            if(in_bout) {
              if(! sleep_window$axis1[i] %in% target) {
                exceptions <- exceptions + 1
                if(exceptions > max_exceptions) {
                  in_bout <- FALSE
                  exceptions <- 0
                  df$bout_stop[nrow(df)] <- i
                  df$bout_stop_time_stamp[nrow(df)] <- sleep_window$time_stamp[i]
                }
              }
            }
          }

          # Update df retain non-wear time bouts with length >= 90 minutes
          df <- df |>
            dplyr::mutate(
              bout_stop = dplyr::case_when(
                is.na(bout_stop) ~ nrow(sleep_window),
                TRUE ~ bout_stop
              ),
              bout_stop_time_stamp = as.POSIXct(bout_stop_time_stamp),
              bout_stop_time_stamp = dplyr::case_when(
                is.na(bout_stop_time_stamp) ~ sleep_window$time_stamp[nrow(sleep_window)],
                TRUE ~ bout_stop_time_stamp
              ),
              length = bout_stop - bout_start
            ) |>
            dplyr::filter(length >= 90)

          # If valid non-wear time bouts exist, set select vectors to "No" for entire sleep window
          if(nrow(df)) {
            # If non-wear time bouts >= 90% of all sleep time
            if(sum(df$length) / nrow(sleep_window) >= 0.90) {
              x2$is_wearing[x2$time_stamp %in% sleep_window$time_stamp] <- "No"
              x2$is_wearing_during_sleep[x2$time_stamp %in% sleep_window$time_stamp] <- "No"
              x2$is_sleeping[x2$time_stamp %in% sleep_window$time_stamp] <- "No"
            } else {
              # Iterate df and update is_wearing_during_sleep
              for(i in 1:nrow(df)) {
                x2 <- x2 |>
                  dplyr::mutate(
                    is_wearing_during_sleep = dplyr::case_when(
                      time_stamp >= df$bout_start_time_stamp[i] & time_stamp < df$bout_stop_time_stamp[i] ~ "No",
                      TRUE ~ is_wearing_during_sleep
                    )
                  )
              }
            }
          }

        } else { x2$is_wearing_during_sleep <- NA }

        return(x2)

      }
    )
  )

  # Look for outliers (any days where bedtime_stop >= 11:59)
  outliers <- x |>
    dplyr::group_by(noon_day) |>
    dplyr::filter(is_sleeping == "Yes") |>
    dplyr::reframe(
      bedtime_stop = dplyr::if_else(
        condition = dplyr::n() > 0,
        true = max(time_stamp),
        false = as.POSIXct(NA)
      )
    ) |>
    dplyr::filter(! is.na(bedtime_stop), hms::as_hms(bedtime_stop) >= hms::as_hms("11:59:00"))

  # If outliers exist, invalidate sleep
  if(nrow(outliers)) {
    for(i in 1:nrow(outliers)) {
      x$is_sleeping[x$noon_day == outliers$noon_day[i]] <- "No"
      x$is_wearing[x$noon_day == outliers$noon_day[i]] <- "No"
      x$is_wearing_during_sleep[x$noon_day == outliers$noon_day[i]] <- NA
      x$sleep_bout[x$noon_day == outliers$noon_day[i]] <- 0
      x$awake_bout[x$noon_day == outliers$noon_day[i]] <- 0
    }
  }

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

  # Determine what to return
  if(return[1] == "everything") return(x) else return(x[return])

}
