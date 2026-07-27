#' @title Classify epochs in an ActiGraph `.agd` file.
#' @description This function classifies epochs in an ActiGraph `.agd` file.
#' @param x Required: an [agd_worker] object.
#' @return Returns `NULL` invisibly.
#' @export

classify_agd_data <- function(x) {
  # Apply sleep algorithm
  if(tolower(x$args$sleep_algo) == "barreira") {
    # Update parameters
    x$args$sleep_epoch_length <- 60

    # Apply Barreira sleep time algorithm
    sleep_output <- x$data$clean |>
      dplyr::filter(epoch_length == 60, filter == "LowFrequencyExtension") |>
      apply_barreira_algo(age = x$args$age, return = c("is_sleeping", "is_wearing", "is_wearing_during_sleep", "ymd_hm", "noon_day", "sleep_bout", "awake_bout"))
  }

  # Create x$data$classify and add sleep variables
  x$data$classify <- x$data$clean |>
    dplyr::left_join(
      y = sleep_output,
      by = "ymd_hm"
    )

  # Update parameters
  x$args$movement_epoch_length <- x$args$epoch_length

  x$args$non_wear_epoch_length <- ifelse(
    test = tolower(x$args$non_wear_algo) == "barreira",
    yes = 60,
    no = x$args$movement_epoch_length
  )

  # Apply non-wear algorithm
  if(tolower(x$args$non_wear_algo) == "barreira") {
    x$data$classify <- x$data$classify |>
      dplyr::select(-dplyr::any_of("is_wearing")) |>
      dplyr::left_join(
        y = sleep_output |>
          dplyr::select(ymd_hm, is_wearing),
        by = "ymd_hm"
      )
  } else if(tolower(x$args$non_wear_algo) %in% paste0(c(20, 60, 90), "-minute")) {
    x$data$classify <- x$data$classify |>
      dplyr::select(-dplyr::any_of("is_wearing")) |>
      dplyr::left_join(
        y = x$data$classify |>
          dplyr::filter(epoch_length == x$args$non_wear_epoch_length, filter == "LowFrequencyExtension") |>
          apply_non_wear_algo(
            min_bout_length = strsplit(x = x$args$non_wear_algo, split = "-")[[1]][1] |> as.integer() * (60 / x$args$non_wear_epoch_length),
            target_values = 0,
            max_exceptions = 2 * ((60 / x$args$non_wear_epoch_length)),
            return = c("is_wearing", "is_sleeping", "ymd_hm")
          ) |>
          dplyr::select(ymd_hm, is_wearing),
        by = "ymd_hm"
      )
  } else if(tolower(x$args$non_wear_algo) == "choi") {
    # Apply Choi (2011) wear time algorithm (https://pubmed.ncbi.nlm.nih.gov/20581716)
    # Note: this function will not work if df is a tibble; df must be of class "data.frame"
    check_name_spaces("PhysicalActivity")

    x$data$classify <- x$data$classify |>
      dplyr::select(-dplyr::any_of("is_wearing")) |>
      dplyr::left_join(
        y = x$data$classify <- x$data$classify |>
          dplyr::select(-dplyr::any_of("is_wearing")) |>
          dplyr::left_join(
            y = PhysicalActivity::wearingMarking(
              dataset = x$data$classify |>
                dplyr::filter(epoch_length == x$args$non_wear_epoch_length, filter == "LowFrequencyExtension"),
              perMinuteCts = 60 / x$args$non_wear_epoch_length,
              TS = "dataTimestamp",
              cts = "axis1"
            ) |>
              dplyr::select(ymd_hm, wearing) |>
              dplyr::mutate(
                wearing = dplyr::case_when(
                  wearing == "w" ~ "Yes",
                  TRUE ~ "No"
                )
              ) |>
              dplyr::rename(is_wearing = wearing),
            by = "ymd_hm"
          ),
        by = "ymd_hm"
      )
  }

  # Get appropriate cut-points movement intensity classification
  if(x$args$age >= 65) {
    sb_cutpoint <- 0 / (60 / x$args$movement_epoch_length)
    lpa_cutpoint <- 100 / (60 / x$args$movement_epoch_length)
    mpa_cutpoint <- 2020 / (60 / x$args$movement_epoch_length)
    vpa_cutpoint <- 5999 / (60 / x$args$movement_epoch_length)
  } else if(x$args$age >= 18) {
    sb_cutpoint <- 0 / (60 / x$args$movement_epoch_length)
    lpa_cutpoint <- 100 / (60 / x$args$movement_epoch_length)
    mpa_cutpoint <- 2020 / (60 / x$args$movement_epoch_length)
    vpa_cutpoint <- 5999 / (60 / x$args$movement_epoch_length)
  } else if(x$args$age >= 5) {
    sb_cutpoint <- 0 / (60 / x$args$movement_epoch_length)
    lpa_cutpoint <- 100 / (60 / x$args$movement_epoch_length)
    mpa_cutpoint <- 2296 / (60 / x$args$movement_epoch_length)
    vpa_cutpoint <- 4012 / (60 / x$args$movement_epoch_length)
  } else if(x$args$age >= 3) {
    sb_cutpoint <- 0 / (60 / x$args$movement_epoch_length)
    lpa_cutpoint <- 100 / (60 / x$args$movement_epoch_length)
    mpa_cutpoint <- vpa_cutpoint <- 1680 / (60 / x$args$movement_epoch_length)
  }

  # Filter data
  x$data$classify <- x$data$classify |>
    dplyr::filter(
      epoch_length == x$args$movement_epoch_length,
      filter == "LowFrequencyExtension"
    )

  # Update parameters
  x$args$sb_cutpoints <- paste0(sb_cutpoint, "-", lpa_cutpoint - 1, " counts per ", x$args$movement_epoch_length, "s", collapse = "")
  x$args$lpa_cutpoints <- paste0(lpa_cutpoint, "-", mpa_cutpoint - 1, " counts per ", x$args$movement_epoch_length, "s", collapse = "")
  x$args$mpa_cutpoints <- paste0(mpa_cutpoint, "-", vpa_cutpoint - 1, " counts per ", x$args$movement_epoch_length, "s", collapse = "")
  x$args$vpa_cutpoints <- paste0(vpa_cutpoint, "+ counts per ", x$args$movement_epoch_length, "s", collapse = "")

  # Classify movement intensity
  x$data$classify <- x$data$classify |>
    dplyr::mutate(
      mvt_class = dplyr::case_when(
        is_wearing == "No" ~ "NW",
        is_sleeping == "Yes" ~ "SL",
        is_sleeping == "No" & awake_bout > 0 ~ "SL",
        x$args$age <= 5 & axis1 >= mpa_cutpoint ~ "MPA",
        axis1 >= vpa_cutpoint ~ "VPA",
        axis1 >= mpa_cutpoint ~ "MPA",
        axis1 >= lpa_cutpoint ~ "LPA",
        axis1 >= sb_cutpoint ~ "SB",
        TRUE ~ NA
      )
    )

  # Add is_mpa_bout
  x$data$classify <- x$data$classify |>
    dplyr::mutate(
      is_mpa_bout = x$data$classify |>
        apply_general_bout_algo(
          min_bout_length = 10 * (60 / unique(epoch_length)),
          target_values = mpa_cutpoint:(vpa_cutpoint - 1),
          max_exceptions = 2 * (60 / unique(epoch_length)),
          return = "in_bout"
        ) |>
        unlist() |>
        unname()
    )

  # Add is_vpa_bout
  x$data$classify <- x$data$classify |>
    dplyr::mutate(
      is_vpa_bout = x$data$classify |>
        apply_general_bout_algo(
          min_bout_length = 10 * (60 / unique(epoch_length)),
          target_values = vpa_cutpoint:19999,
          max_exceptions = 2 * (60 / unique(epoch_length)),
          return = "in_bout"
        ) |>
        unlist() |>
        unname()
    )

  # Add is_mvpa_bout
  x$data$classify <- x$data$classify |>
    dplyr::mutate(
      is_mvpa_bout = x$data$classify |>
        apply_general_bout_algo(
          min_bout_length = 10 * (60 / unique(epoch_length)),
          target_values = mpa_cutpoint:19999,
          max_exceptions = 2 * (60 / unique(epoch_length)),
          return = "in_bout"
        ) |>
        unlist() |>
        unname()
    )

  # Exit
  return(invisible(NULL))
}
