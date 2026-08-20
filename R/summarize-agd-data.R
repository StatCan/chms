#' @title Summarizes an ActiGraph `.agd` file.
#' @description This function summarizes an ActiGraph `.agd` file.
#' @param x Required: an [agd_worker] object.
#' @return Returns a `NULL` invisibly.
#' @examples \donttest{
#' # Initialize agd_worker R6 class
#' agd_data <- agd_worker$new(
#'   id = "jane-canuck",
#'   age = 10,
#'   agd_lfe = system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
#'   agd_nml = system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
#'   epoch_length = 15,
#'   day_max = 7,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira"
#' )
#'
#' # Load, clean and classify data
#' agd_data$load()$clean()$classify()
#'
#' # Summarize data
#' summarize_agd_data(agd_data)
#'
#' # Store updated data
#' dt <- agd_data$results$summary_full
#' }
#' @export

summarize_agd_data <- function(x) {
  # Add midnight day of the week, noon day and sleep bouts
  output <- x$data$classify |>
    dplyr::mutate(
      hour = lubridate::hour(dataTimestamp),
      midnight_day_of_week = weekdays(dataTimestamp),
      noon_day = ifelse(
        test = hour < 12,
        yes = midnight_day - 1,
        no = midnight_day
      ) |> as.integer(),
    ) |>
    dplyr::select(-hour, -filter) |>
    dplyr::relocate(midnight_day_of_week, noon_day, .after = midnight_day)

  # Add noon_day_of_week
  output <- dplyr::left_join(
    x = output,
    y = dplyr::bind_rows(
      lapply(
        X = unique(output$noon_day),
        FUN = function(x) {
          dplyr::tibble(
            noon_day = x,
            noon_day_of_week = paste0(
              unique(output$midnight_day_of_week[output$noon_day == x]),
              collapse = "-"
            )
          )
        }
      )
    ),
    by = "noon_day"
  ) |>
    dplyr::relocate(noon_day_of_week, .after = noon_day)

  # Summarize data from midnight
  day_level_summary_from_midnight <- output |>
    dplyr::group_by(midnight_day) |>
    dplyr::reframe(
      participant_id = unique(x$args$id),
      device_serial_number = x$data$settings$deviceserial,
      participant_age = x$args$age,
      day_of_week = unique(midnight_day_of_week),
      wear_time = sum(! mvt_class %in% c("NW", "SL")) / (60 / unique(output$epoch_length)) / 60,
      steps = sum(steps_normal),
      steps_lfe = sum(steps_lfe),
      mpa = sum(mvt_class == "MPA") / (60 / unique(output$epoch_length)),
      vpa = sum(mvt_class == "VPA") / (60 / unique(output$epoch_length)),
      mvpa_bouts = sum(is_mvpa_bout == "Yes") / (60 / unique(output$epoch_length)),
      vpa_bouts = sum(is_vpa_bout == "Yes") / (60 / unique(output$epoch_length)),
      mpa_bouts = sum(is_mpa_bout == "Yes") / (60 / unique(output$epoch_length)),
      mvpa = sum(mpa, vpa),
      lpa = sum(mvt_class == "LPA") / (60 / unique(output$epoch_length)),
      lmvpa = sum(lpa, mvpa),
      sb = sum(mvt_class == "SB") / (60 / unique(output$epoch_length))
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(day = midnight_day) |>
    dplyr::relocate(day, .after = participant_age) |>
    dplyr::mutate(
      valid_day = dplyr::case_when(
        wear_time >= 10 & steps >= 100 ~ 1,
        TRUE ~ 0
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      steps_predicted = dplyr::case_when(
        ! is.na(steps) & participant_age %in% 3:5 ~ max(
          0,
          -3090.2251056 +
            0.4791724 * steps_lfe +
            724.4322622 * participant_age -
            5.4615174 * lpa +
            17.9441556 * mpa -
            1.4267246 * sb -
            14.2077226 * wear_time
        ),
        ! is.na(steps) & participant_age %in% 6:17 ~ max(
          0,
          -973.0051637 +
            0.5121848 * steps_lfe +
            86.3428694 * participant_age -
            8.6813245 * lpa +
            49.1781703 * mpa +
            30.7498738 * vpa -
            20.6189735 * wear_time
        ),
        ! is.na(steps) & participant_age %in% 18:64 ~ max(
          0,
          -48.6782192 +
            0.3841127 * steps_lfe +
            0.1393666 * lpa +
            53.1375913 * mpa +
            83.0469805 * vpa -
            0.6426106 * sb
        ),
        ! is.na(steps) & participant_age %in% 65:80 ~ max(
          0,
          3195.5622610 +
            0.3308764 * steps_lfe -
            41.1554648 * participant_age +
            1.0939458 * lpa +
            66.8891181 * mpa +
            78.5750254 * vpa -
            1.1717213 * sb
        ),
        TRUE ~ NA
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(steps_predicted, .after = steps)

  # Aggregate summary data from midnight (valid days only)
  if(sum(day_level_summary_from_midnight$valid_day) == 0) {
    participant_level_summary_from_midnight <- dplyr::tibble(
      participant_id = unique(day_level_summary_from_midnight$participant_id),
      device_serial_number = unique(day_level_summary_from_midnight$device_serial_number),
      participant_age = mean(day_level_summary_from_midnight$participant_age),
      wear_time = NA,
      steps = NA,
      steps_predicted = NA,
      steps_lfe = NA,
      mvpa_bouts = NA,
      vpa_bouts = NA,
      mpa_bouts = NA,
      mvpa = NA,
      vpa = NA,
      mpa = NA,
      lpa = NA,
      lmvpa = NA,
      sb = NA,
      valid_day = 0
    )
  } else {
    participant_level_summary_from_midnight <- day_level_summary_from_midnight |>
      dplyr::filter(valid_day == 1) |>
      dplyr::group_by(participant_id) |>
      dplyr::reframe(
        participant_id = unique(participant_id),
        device_serial_number = unique(device_serial_number),
        participant_age = mean(participant_age),
        wear_time = mean(wear_time),
        steps = mean(steps),
        steps_predicted = mean(steps_predicted),
        steps_lfe = mean(steps_lfe),
        mvpa_bouts = dplyr::if_else(
          condition = is.nan(mean(mvpa_bouts[mvpa_bouts > 0])),
          true = 0,
          false = mean(mvpa_bouts[mvpa_bouts > 0])
        ),
        vpa_bouts = dplyr::if_else(
          condition = is.nan(mean(vpa_bouts[vpa_bouts > 0])),
          true = 0,
          false = mean(vpa_bouts[vpa_bouts > 0])
        ),
        mpa_bouts = dplyr::if_else(
          condition = is.nan(mean(mpa_bouts[mpa_bouts > 0])),
          true = 0,
          false = mean(mpa_bouts[mpa_bouts > 0])
        ),
        mvpa = mean(mvpa),
        vpa = mean(vpa),
        mpa = mean(mpa),
        lpa = mean(lpa),
        lmvpa = mean(lmvpa),
        sb = mean(sb),
        valid_day = sum(valid_day)
      )
  }

  # Add to x$results$summary_waking_hours
  x$results$summary_waking_hours <- dplyr::bind_rows(
    day_level_summary_from_midnight |>
      dplyr::mutate(summary = "Waking hours") |>
      dplyr::relocate(summary),
    participant_level_summary_from_midnight |>
      dplyr::mutate(summary = "Waking hours (average)") |>
      dplyr::relocate(summary)
  )

  # Summarize data from noon
  day_level_summary_from_noon <- output |>
    dplyr::group_by(noon_day) |>
    dplyr::reframe(
      participant_id = unique(x$args$id),
      device_serial_number = x$data$settings$deviceserial,
      participant_age = x$args$age,
      day_of_week = unique(noon_day_of_week),
      wear_time = sum(is_wearing_during_sleep == "Yes", na.rm = TRUE) / (60 / unique(output$epoch_length)) / 60,
      nocturnal_sleep_onset = suppressWarnings(min(dataTimestamp[mvt_class == "SL"], na.rm = TRUE)),
      nocturnal_sleep_offset = suppressWarnings(max(dataTimestamp[mvt_class == "SL"], na.rm = TRUE)),
      nocturnal_sleep_midpoint = suppressWarnings(stats::median(dataTimestamp[mvt_class == "SL"], na.rm = TRUE)),
      sleep_period_time = sum(mvt_class == "SL") / (60 / unique(output$epoch_length)) / 60,
      sleep_episodes = max(sleep_bout, na.rm = TRUE),
      wake_episodes = max(awake_bout, na.rm = TRUE),
      total_wake_episode_time = sum(awake_bout > 0) / (60 / unique(output$epoch_length)) / 60,
      total_sleep_episode_time = sleep_period_time - total_wake_episode_time,
      sleep_episode_efficiency = total_sleep_episode_time / sleep_period_time,
      total_restful_sleep_time = sum(axis1[mvt_class == "SL" & awake_bout == 0] == 0) / (60 / unique(output$epoch_length)) / 60,
      sleep_episode_movements = sum(axis1[mvt_class == "SL" & awake_bout == 0] > 0) / (60 / unique(output$epoch_length)) / 60,
      total_disrupted_sleep = sum(axis1[mvt_class == "SL"] > 0) / (60 / unique(output$epoch_length)) / 60,
      restful_sleep_efficiency = total_restful_sleep_time / sleep_period_time
    ) |>
    suppressWarnings() |>
    dplyr::ungroup() |>
    dplyr::rename(day = noon_day) |>
    dplyr::relocate(day, .after = participant_age) |>
    dplyr::mutate(
      valid_day = dplyr::case_when(
        stringr::str_detect(day_of_week, "-") & sleep_period_time >= 160 / 60 ~ 1,
        TRUE ~ 0
      ),
      dplyr::across(
        .cols = c(nocturnal_sleep_onset:restful_sleep_efficiency),
        .fns = ~ dplyr::case_when(
          valid_day == 0 ~ NA,
          TRUE ~ .x
        )
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(valid_day = sum(valid_day))

  # Aggregate summary data from noon (valid days only)
  if(sum(day_level_summary_from_noon$valid_day) == 0) {
    participant_level_summary_from_noon <- dplyr::tibble(
      participant_id = unique(day_level_summary_from_noon$participant_id),
      device_serial_number = unique(day_level_summary_from_noon$device_serial_number),
      participant_age = mean(day_level_summary_from_noon$participant_age),
      wear_time = NA,
      nocturnal_sleep_onset = NA,
      nocturnal_sleep_offset = NA,
      nocturnal_sleep_midpoint = NA,
      sleep_period_time = NA,
      sleep_episodes = NA,
      wake_episodes = NA,
      total_wake_episode_time = NA,
      total_sleep_episode_time = NA,
      sleep_episode_efficiency = NA,
      total_restful_sleep_time = NA,
      sleep_episode_movements = NA,
      total_disrupted_sleep = NA,
      restful_sleep_efficiency = NA,
      valid_day = 0
    )
  } else {
    participant_level_summary_from_noon <- day_level_summary_from_noon |>
      dplyr::filter(valid_day == 1) |>
      dplyr::group_by(participant_id) |>
      dplyr::reframe(
        device_serial_number = unique(device_serial_number),
        participant_age = unique(participant_age),
        wear_time = mean(wear_time),
        dplyr::across(
          .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
          .fns = ~ .x |>
            na.omit() |>
            hms::as_hms() |>
            as.numeric() |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              value = dplyr::case_when(
                value < 3600 * 12 ~ value + 86400,
                TRUE ~ value
              )
            ) |>
            dplyr::reframe(mean = mean(value)) |>
            dplyr::mutate(mean = as.POSIXct(x = paste("1970-01-01", hms::as_hms(mean %% 86400)), tz = "UTC")) |>
            dplyr::pull()
        ),
        dplyr::across(
          .cols = sleep_period_time:restful_sleep_efficiency,
          .fns = ~ mean(.x)
        ),
        valid_day = sum(valid_day)
      )
  }

  # Add to x$results
  x$results$summary_sleeping_hours <- dplyr::bind_rows(
    day_level_summary_from_noon |>
      dplyr::mutate(summary = "Sleeping hours") |>
      dplyr::relocate(summary),
    participant_level_summary_from_noon |>
      dplyr::mutate(summary = "Sleeping hours (average)") |>
      dplyr::relocate(summary)
  ) |>
    dplyr::left_join(
      y = dplyr::bind_rows(
        output |>
          dplyr::group_by(noon_day, sleep_bout) |>
          dplyr::reframe(
            duration = dplyr::n() / (60 / unique(output$epoch_length)),
            start_time = min(ymd_hm),
            stop_time = max(ymd_hm)
          ),
        output |>
          dplyr::group_by(noon_day, awake_bout) |>
          dplyr::reframe(
            duration = dplyr::n() / (60 / unique(output$epoch_length)),
            start_time = min(ymd_hm),
            stop_time = max(ymd_hm)
          )
      ) |>
        dplyr::mutate(
          day_of_week = dplyr::case_when(
            weekdays(start_time) == weekdays(stop_time) ~ substr(x = tolower(weekdays(start_time)), start = 1, stop = 3),
            TRUE ~ paste0(
              substr(x = tolower(weekdays(start_time)), start = 1, stop = 3),
              "-",
              substr(x = tolower(weekdays(stop_time)), start = 1, stop = 3)
            )
          ),
          episode = dplyr::case_when(
            ! is.na(sleep_bout) ~ "sleep episode",
            TRUE ~ "awake episode"
          ),
          number = dplyr::coalesce(sleep_bout, awake_bout)
        ) |>
        dplyr::arrange(start_time) |>
        dplyr::select(-sleep_bout, -awake_bout) |>
        dplyr::relocate(noon_day, day_of_week, episode, number) |>
        dplyr::ungroup() |>
        dplyr::group_by(noon_day) |>
        dplyr::reframe(
          sleep_episode_log = jsonlite::toJSON(
            x = dplyr::filter(dplyr::pick(dplyr::everything()), number > 0),
            dataframe = "rows",
            auto_unbox = TRUE
          )
        ) |>
        dplyr::mutate(sleep_episode_log = as.character(sleep_episode_log)) |>
        dplyr::rename(day = noon_day),
      by = "day"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sleep_episode_log = dplyr::case_when(
        is.na(nocturnal_sleep_onset) ~ as.character(jsonlite::toJSON("")),
        TRUE ~ sleep_episode_log
      )
    )

  # Bind waking hours and sleeping hours summaries and add to x$data
  x$results$summary_full <- dplyr::bind_rows(
    x$results$summary_waking_hours,
    x$results$summary_sleeping_hours
  ) |>
    dplyr::relocate(nocturnal_sleep_onset:restful_sleep_efficiency, .after = sb)

  # Create variable lookup table
  variable_lookup <- data.frame(
    name = c(
      "participant_id",
      "participant_id",
      "day",
      "day",
      "day_of_week",
      "wear_time",
      "wear_time",
      "valid_day",
      "nocturnal_sleep_onset",
      "nocturnal_sleep_offset",
      "nocturnal_sleep_midpoint",
      "sleep_period_time",
      "sleep_episodes",
      "wake_episodes",
      "total_wake_episode_time",
      "total_sleep_episode_time",
      "sleep_episode_efficiency",
      "total_restful_sleep_time",
      "sleep_episode_movements",
      "total_disrupted_sleep",
      "restful_sleep_efficiency",
      "valid_day",
      "steps",
      "sb",
      "lpa",
      "mpa",
      "vpa",
      "mvpa",
      "lmvpa",
      "mpa_bouts",
      "vpa_bouts",
      "mvpa_bouts"
    ),
    level = c(
      "Sleeping hours",
      "Waking hours",
      "Sleeping hours",
      "Waking hours",
      "Waking hours",
      "Sleeping hours",
      "Waking hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Sleeping hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours",
      "Waking hours"
    ),
    sspe_name = c(
      "clinicid",
      "clinicid",
      "day_sleep",
      "amgdt",
      "amgddow",
      "amgdhrs",
      "amgdhr",
      "amgdvls",
      "amgdbt",
      "amgdwt",
      "amgdmbwt",
      "amgdslp",
      "amgdnse",
      "amgdwse",
      "amgdwet",
      "amgdset",
      "amgdsee",
      "amgdrst",
      "amgdsem",
      "amgdtds",
      "amgdrse",
      "amgdval",
      "amgdsst",
      "amgdsa",
      "amgdla",
      "amgdma",
      "amgdva",
      "amgdmva",
      "amgdta",
      "amgdmb",
      "amgdvb",
      "amgdmvb"
    ),
    class = c(
      "character",
      "character",
      "integer",
      "integer",
      "character",
      "numeric",
      "numeric",
      "integer",
      "date",
      "date",
      "date",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "integer",
      "integer",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    digits = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      2,
      2,
      NA,
      NA,
      NA,
      NA,
      1,
      1,
      1,
      1,
      1,
      2,
      1,
      1,
      1,
      2,
      NA,
      NA,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1
    )
  ) |>
    dplyr::left_join(
      y = data.frame(name = names(x$results$summary_full)),
      by = "name"
    )

  # Get max day
  max_day <- max(x$results$summary_full$day, na.rm = TRUE)

  # Reshape x$results$summary_full for StatCan and bind to x$results$summary_full_stc
  x$results$summary_full_stc <- dplyr::left_join(
    x = x$results$summary_full |>
      dplyr::filter(stringr::str_detect(summary, "Sleeping hours")) |>
      dplyr::mutate(
        dplyr::across(
          .cols = nocturnal_sleep_onset:nocturnal_sleep_offset,
          .fns = ~ format(
            x = .x,
            format = "%Y-%m-%d %H:%M:%S"
          )
        )
      ) |>
      dplyr::select(variable_lookup$name[variable_lookup$level == "Sleeping hours"]) |>
      dplyr::rename_with(~ variable_lookup$sspe_name[variable_lookup$level == "Sleeping hours"]) |>
      dplyr::mutate(
        day = !! rlang::sym(variable_lookup$sspe_name[variable_lookup$level == "Sleeping hours" & variable_lookup$name == "day"]),
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ as.character(.x)
        ),
        day = dplyr::case_when(
          is.na(day) ~ "",
          TRUE ~ day
        )
      ) |>
      dplyr::relocate(day) |>
      dplyr::filter(day != "0"),
    y = x$results$summary_full |>
      dplyr::filter(stringr::str_detect(summary, "Waking hours")) |>
      dplyr::select(variable_lookup$name[variable_lookup$level == "Waking hours"]) |>
      dplyr::rename_with(~ variable_lookup$sspe_name[variable_lookup$level == "Waking hours"]) |>
      dplyr::mutate(
        day = !! rlang::sym(variable_lookup$sspe_name[variable_lookup$level == "Waking hours" & variable_lookup$name == "day"]),
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ as.character(.x)
        ),
        day = dplyr::case_when(
          is.na(day) ~ "",
          TRUE ~ day
        )
      ) |>
      dplyr::relocate(day),
    by = c("clinicid", "day")
  ) |>
    dplyr::select(
      c(
        day,
        variable_lookup |>
          dplyr::filter(sspe_name != "day_sleep") |>
          dplyr::select(sspe_name) |>
          dplyr::distinct() |>
          unlist() |>
          unname()
      )
    ) |>
    tidyr::pivot_longer(
      cols = -c(clinicid, day),
      cols_vary = "slowest"
    ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        tolower(value) == "yes" ~ "1",
        tolower(value) == "no" ~ "0",
        TRUE ~ value
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = clinicid,
      names_from = c(name, day),
      names_sep = "",
      values_from = value
    ) |>
    dplyr::mutate(
      amgdvls_avg = rowSums(
        x = dplyr::across(
          .cols = dplyr::contains("amgdvls"),
          .fns = ~ .x == "1"
        ),
        na.rm = TRUE
      ),
      amgdval_avg = rowSums(
        x = dplyr::across(
          .cols = dplyr::contains("amgdval"),
          .fns = ~ .x == "1"
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::select(
      -amgdt,
      -amgddow,
      -amgdvls,
      -amgdval
    ) |>
    dplyr::rename(
      amgdvls = amgdvls_avg,
      amgdval = amgdval_avg
    ) |>
    dplyr::relocate(amgdvls, .after = dplyr::contains(paste0("amgdvls", max_day))) |>
    dplyr::relocate(amgdval, .after = dplyr::contains(paste0("amgdval", max_day))) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ {
          # Get vector name
          # Strip out any numbers
          vector_name <- gsub(
            pattern = "\\d+",
            replacement = "",
            x = dplyr::cur_column()
          )

          # Get vector character length
          vector_length <- nchar(vector_name)

          # Get vector meta
          vector_meta <- variable_lookup |>
            dplyr::filter(
              nchar(sspe_name) == vector_length,
              stringr::str_detect(
                string = sspe_name,
                pattern = vector_name
              )
            ) |>
            dplyr::select(class, digits) |>
            dplyr::distinct() |>
            unlist()

          # Format vectors
          if(length(vector_meta)) {
            if(vector_meta["class"] == "integer") {
              as.integer(.x)
            } else if(vector_meta["class"] == "numeric") {
              as.numeric(.x) |>
                round(digits = as.integer(vector_meta["digits"]))
            } else if(vector_meta["class"] == "date") {
              format(
                x = lubridate::ymd_hms(.x),
                format = "%Y-%m-%d %H:%M"
              )
            } else { .x }
          } else { .x }
        }
      ),
      amgdpwa = dplyr::case_when(
        x$args$age %in% 18:80 & amgdval >= 4 & amgdmva >= (150 / 7) ~ 1,
        x$args$age %in% 18:80 & amgdval >= 4 & amgdmva < (150 / 7) ~ 2,
        TRUE ~ NA
      ),
      amgdadk = dplyr::case_when(
        x$args$age %in% 5:17 & amgdval >= 4 & amgdmva >= 60 ~ 1,
        x$args$age %in% 5:17 & amgdval >= 4 & amgdmva < 60 ~ 2,
        TRUE ~ NA
      ),
      amgdpdp = dplyr::case_when(
        x$args$age %in% 3:4 & amgdval >= 4 & amgdmva >= 60 ~ 1,
        x$args$age %in% 3:4 & amgdval >= 4 & amgdmva < 60 ~ 2,
        TRUE ~ NA
      ),
      amgdsda = dplyr::case_when(
        x$args$age %in% 18:80 & amgdvls >= 3 & amgdslp >= 7 & amgdslp < 10 ~ 1,
        x$args$age %in% 18:80 & amgdvls >= 3 & (amgdslp < 7 | amgdslp >= 10) ~ 2,
        TRUE ~ NA
      ),
      amgdsdk = dplyr::case_when(
        x$args$age %in% 5:13 & amgdvls >= 3 & amgdslp >= 9 & amgdslp < 12 ~ 1,
        x$args$age %in% 5:13 & amgdvls >= 3 & (amgdslp < 9 | amgdslp >= 12) ~ 2,
        x$args$age %in% 14:17 & amgdvls >= 3 & amgdslp >= 8 & amgdslp < 11 ~ 1,
        x$args$age %in% 14:17 & amgdvls >= 3 & (amgdslp < 8 | amgdslp >= 11) ~ 2,
        TRUE ~ NA
      ),
      amgdsdp = dplyr::case_when(
        x$args$age %in% 3:4 & amgdvls >= 3 & amgdslp >= 10 & amgdslp < 14 ~ 1,
        x$args$age %in% 3:4 & amgdvls >= 3 & (amgdslp < 10 | amgdslp >= 14) ~ 2,
        TRUE ~ NA
      )
    ) |>
    dplyr::rename_with(toupper)

  # Ensure AMGDT variables have no missing values
  amgdt_imputation <- x$results$summary_full_stc |>
    dplyr::select(CLINICID, matches("^AMGDT(\\d*)$")) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("AMGDT")) |>
    dplyr::group_by(CLINICID) |>
    dplyr::mutate(
      value = purrr::accumulate(
        .x = value,
        .f = ~ ifelse(
          test = is.na(.y),
          yes = .x + 1,
          no = .y
        )
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(id_cols = CLINICID)

  # Make on-the-fly function for processing AMGDDOW variables
  get_next_day <- function(day) {
    days <- weekdays(as.Date("2025-01-01") + 1:7)
    match <- match(day, days)
    if(! is.na(match)) {
      days[(match %% length(days)) + 1]
    } else { NA }
  }

  # Ensure AMGDDOW variables have no missing values
  amgddow_imputation <- x$results$summary_full_stc |>
    dplyr::select(CLINICID, dplyr::starts_with("AMGDDOW")) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("AMGDDOW")) |>
    dplyr::group_by(CLINICID) |>
    dplyr::mutate(
      value = purrr::accumulate(
        .x = value,
        .f = ~ ifelse(
          test = is.na(.y),
          yes = get_next_day(.x),
          no = .y
        )
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(id_cols = CLINICID)

  # Join imputed values
  x$results$summary_full_stc <- dplyr::left_join(
    x = x$results$summary_full_stc |>
      dplyr::select(! matches("^AMGDT(\\d*)$"), -dplyr::starts_with("AMGDDOW")),
    y = dplyr::left_join(
      x = amgdt_imputation,
      y = amgddow_imputation,
      by = "CLINICID"
    ),
    by = "CLINICID"
  )

  # Reorder vectors in summary_full_statcan_format for Processing
  x$results$summary_full_stc <- x$results$summary_full_stc |>
    dplyr::relocate(
      dplyr::any_of(
        c(
          "CLINICID",
          paste0("AMGDT", 1:7),
          paste0("AMGDDOW", 1:7),
          c(paste0("AMGDHR", 1:7), "AMGDHR"),
          c(paste0("AMGDSLP", 1:7), "AMGDSLP"),
          c(paste0("AMGDSST", 1:7), "AMGDSST"),
          c(paste0("AMGDMA", 1:7), "AMGDMA"),
          c(paste0("AMGDVA", 1:7), "AMGDVA"),
          c(paste0("AMGDMVB", 1:7), "AMGDMVB"),
          c(paste0("AMGDVB", 1:7), "AMGDVB"),
          c(paste0("AMGDMB", 1:7), "AMGDMB"),
          c(paste0("AMGDMVA", 1:7), "AMGDMVA"),
          c(paste0("AMGDLA", 1:7), "AMGDLA"),
          c(paste0("AMGDTA", 1:7), "AMGDTA"),
          c(paste0("AMGDSA", 1:7), "AMGDSA"),
          c(paste0("AMGDVAL", 1:7), "AMGDVAL"),
          c(paste0("AMGDVLS", 1:7), "AMGDVLS"),
          "AMGDPWA",
          "AMGDADK",
          "AMGDPDP",
          "AMGDSDA",
          "AMGDSDK",
          "AMGDSDP",
          c(paste0("AMGDBT", 1:7), "AMGDBT"),
          c(paste0("AMGDWT", 1:7), "AMGDWT"),
          c(paste0("AMGDHRS", 1:7), "AMGDHRS")
        )
      )
    )

  # Convert datetime vectors to character vectors
  x$results$summary_sleeping_hours <- x$results$summary_sleeping_hours |>
    dplyr::mutate(
      dplyr::across(
        .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
        .fns = ~ as.character(.x)
      ),
      dplyr::across(
        .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
        .fns = ~ gsub(pattern = "1970-01-01 ", replacement = "", x = .x)
      ),
      dplyr::across(
        .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
        .fns = ~ dplyr::case_when(
          .x == "1970-01-01" ~ "00:00:00",
          TRUE ~ .x
        )
      )
    )

  x$results$summary_full <- x$results$summary_full |>
    dplyr::mutate(
      dplyr::across(
        .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
        .fns = ~ as.character(.x)
      ),
      dplyr::across(
        .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
        .fns = ~ gsub(pattern = "1970-01-01 ", replacement = "", x = .x)
      ),
      dplyr::across(
        .cols = nocturnal_sleep_onset:nocturnal_sleep_midpoint,
        .fns = ~ dplyr::case_when(
          .x == "1970-01-01" ~ "00:00:00",
          TRUE ~ .x
        )
      )
    )

  # Exit
  return(invisible(NULL))
}
