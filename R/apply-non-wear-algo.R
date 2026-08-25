#' @title Classify an accelerometer axis vector of epochs as wear time or non-wear time based on a set of bout rules.
#' @description This function uses a general algorithm to classify an accelerometer axis vector of epochs as wear time or non-wear time based on a set of bout rules
#' @param x Required: a data frame of accelerometer data.
#' @param axis1 Required: a length-one character vector representing the name of the vertical axis (default: "axis1").
#' @param min_bout_length Required: a length-one numeric vector representing the minimum number of epochs in a bout (default: 10).
#' @param target_values Required: a numeric vector representing the epoch value(s) that belong to a bout.
#' @param max_exceptions Required: a length-one numeric vector representing the maximum number of epochs permitted within a bout that outside the range of the values in the `target_values` argument (default: 2).
#' @param return Required: a character vector representing which vectors to return (default: "everything"). If set to "everything", the data frame in the `x` argument will be returned along with all vectors that were derived while applying the bout algorithm.
#' @return Returns the data frame in the `x` argument along with all vectors that were derived while applying the bout algorithm.
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
#' # Apply Barreira sleep algorithm
#' dt <- apply_barreira_algo(
#'   x = agd_data$data$clean |>
#'     dplyr::filter(
#'       epoch_length == 60,
#'       filter == "LowFrequencyExtension"
#'     ),
#'   age = agd_data$args$age
#' )
#'
#' # Apply general non-wear algorithm
#' dt <- apply_non_wear_algo(
#'   x = dt,
#'   min_bout_length = 60,
#'   target_values = 0,
#'   max_exceptions = 2,
#'   return = c("is_wearing", "is_sleeping", "ymd_hm")
#' )
#' @export

apply_non_wear_algo <- function(
    x,
    axis1 = "axis1",
    min_bout_length = 90,
    target_values,
    max_exceptions = 2,
    return = "everything"
) {
  # Rename vectors
  x <- x |>
    dplyr::rename(axis1 = dplyr::all_of(axis1))

  # Classify bouts of a specified intensity by day (12:00am to 11:59pm)
  x <- dplyr::bind_rows(
    lapply(
      X = unique(x$ymd),
      FUN = function(y) {

        # Filter on ymd and create empty in_bout vector
        x2 <- x |>
          dplyr::filter(ymd == y) |>
          dplyr::mutate(in_bout = NA)

        # Create data frame of run lengths
        df <- x2 |>
          dplyr::mutate(
            in_bout = dplyr::case_when(
              axis1 %in% target_values & is_sleeping == "No" ~ "Yes",
              TRUE ~ "No"
            )
          ) |>
          dplyr::select(in_bout) |>
          unlist() |>
          unname() |>
          rle() |>
          "class<-"("list") |>
          as.data.frame()

        # Set default values for variables to be used in loop below
        in_bout <- FALSE
        target_value_count <- 0
        exceptions_count <- 0
        rows <- NULL

        # Iterate df
        for(i in 1:nrow(df)) {

          # If a run length equals "Yes", i.e. a length of epochs with counts that are within target_values argument
          if(df$values[i] == "Yes") {

            if(! in_bout) in_bout <- TRUE

            target_value_count <- target_value_count + df$lengths[i]
            rows <- append(rows, i)

            # Else, the run length equals "No", i.e. a length of epochs with counts outside target_values argument
          } else {

            if(in_bout) {

              exceptions_count <- exceptions_count + df$lengths[i]

              if(exceptions_count > max_exceptions) {

                if(target_value_count >= min_bout_length - max_exceptions) {

                  bout_row_start <- sum(df$lengths[1:(min(rows) - 1)]) + 1
                  bout_row_stop <- sum(df$lengths[1:max(rows)])
                  exceptions_count <- sum(! x2$axis1[bout_row_start:bout_row_stop] %in% target_values)
                  bout_row_stop <- bout_row_stop + (max_exceptions - exceptions_count)
                  x2$in_bout[bout_row_start:bout_row_stop] <- "Yes"

                }

                in_bout <- FALSE
                target_value_count <- 0
                exceptions_count <- 0
                rows <- NULL

              } else {

                rows <- append(rows, i)

              }

            }

          }

        }

        # Update in_bout vector
        x2 <- x2 |>
          dplyr::mutate(
            is_wearing = dplyr::case_when(
              in_bout == "Yes" ~ "No",
              TRUE ~ "Yes"
            )
          )

        return(x2)

      }
    )
  )

  # Restore original vector names
  x <- x |> dplyr::rename(!! axis1 := axis1)

  # Determine what to return and exit
  if("everything" %in% return) return(x) else return(x[return])
}
