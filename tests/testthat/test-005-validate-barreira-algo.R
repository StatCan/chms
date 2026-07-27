test_that("Barreira algo in R matches Barreira algo in SAS", {
  # Skip if configs not found
  testthat::skip_if_not(nzchar(config::get("cycle7_clinic_file")))
  testthat::skip_if_not(nzchar(config::get("cycle7_site2_agd_dir_lfe")))
  testthat::skip_if_not(nzchar(config::get("cycle7_site2_agd_dir_nml")))
  testthat::skip_if_not(nzchar(config::get("cycle7_site2_sas_results")))

  # Load custom functions
  compare_times <- function(x, y) {
    # Format times
    x <- as.POSIXct(x, format = "%H:%M:%S")
    y <- as.POSIXct(y, format = "%H:%M:%S")

    # Add 24 hours if time is in the morning
    x <- x + ifelse(lubridate::hour(x) <= 11, 86400, 0)
    y <- y + ifelse(lubridate::hour(y) <= 11, 86400, 0)

    # Exit
    return(as.numeric(abs(difftime(x, y, units = "mins"))))
  }

  compare_sleep <- function(id, tolerance = 5) {
    # Get SAS case data and format
    sas_case <- sas |>
      dplyr::filter(pid == {id}) |>
      dplyr::mutate(
        day = substr(x = tolower(weekdays(date)), start = 1, stop = 3),
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ as.character(.x)
        )
      ) |>
      dplyr::select(-date, -noon, -paxday) |>
      dplyr::rename(sleep_period_time = tot_sleepnw, total_wake_episode_time = tot_nw, total_sleep_episode_time = tot_sleep) |>
      dplyr::relocate(pid, day, bedtime, waketime, sleep_period_time, total_wake_episode_time, total_sleep_episode_time) |>
      tidyr::pivot_longer(
        cols = bedtime:total_sleep_episode_time,
        names_to = "variable",
        values_to = "sas_value"
      )

    # Get R case data and format
    r_case <- r |>
      dplyr::filter(participant_id == {id}, day > 0, day < 7) |>
      dplyr::select(participant_id, day_of_week, nocturnal_sleep_onset:nocturnal_sleep_offset, sleep_period_time, total_wake_episode_time, total_sleep_episode_time) |>
      dplyr::rename(
        pid = 1,
        day = 2,
        bedtime = 3,
        waketime = 4
      ) |>
      dplyr::mutate(
        day = tolower(stringr::str_sub(day, 1, 3)),
        dplyr::across(
          .cols = bedtime:waketime,
          .fns = ~ stringr::str_extract(
            string = ifelse(
              test = ! is.na(.x) & ! stringr::str_detect(string = .x, pattern = ":"),
              yes = paste(.x, "00:00:00"),
              no = .x
            ),
            pattern = "(?<=\\s).*"
          )
        ),
        dplyr::across(
          .cols = sleep_period_time:total_sleep_episode_time,
          .fns = ~ as.character(trunc((.x * 60) * 100) / 100)
        )
      ) |>
      tidyr::pivot_longer(
        cols = bedtime:total_sleep_episode_time,
        names_to = "variable",
        values_to = "r_value"
      )

    # Join data
    case <- sas_case |>
      dplyr::filter(! variable %in% c("total_wake_episode_time", "sleep_period_time")) |>
      dplyr::mutate(variable = ifelse(variable == "total_sleep_episode_time", "tot_sleep vs. sleep_period_time", variable)) |>
      dplyr::left_join(
        y = r_case |>
          dplyr::filter(! variable %in% c("total_wake_episode_time", "total_sleep_episode_time")) |>
          dplyr::mutate(variable = ifelse(variable == "sleep_period_time", "tot_sleep vs. sleep_period_time", variable)),
        by = c("pid", "day", "variable")
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        delta = dplyr::case_when(
          sum(is.na(c(sas_value, r_value))) == 2 ~ NA,
          sum(is.na(c(sas_value, r_value))) == 1 ~ 24,
          variable %in% c("bedtime", "waketime") ~ compare_times(sas_value, r_value),
          variable %in% c("sleep_period_time", "total_wake_episode_time", "total_sleep_episode_time") ~ suppressWarnings(abs(as.numeric(sas_value) - as.numeric(r_value))),
          TRUE ~ NA
        ),
        delta = ifelse(
          test = is.na(delta),
          yes = NA,
          no = sprintf(fmt = "%.2f", delta)
        ),
        delta = dplyr::case_when(
          delta == "NA" ~ delta,
          as.numeric(delta) > tolerance & as.numeric(delta) == 24 ~ paste0("<span style='background: red; color: white; font-weight: bold; height: 100%; width: 100%;'>", paste0(sprintf(fmt = "%.0f", tolerance), "+"), "</span>"),
          as.numeric(delta) > tolerance ~ paste0("<span style='background: red; color: white; font-weight: bold; height: 100%; width: 100%;'>", delta, "</span>"),
          TRUE ~ delta
        ),
        dplyr::across(
          .cols = sas_value:delta,
          .fns = ~ ifelse(test = is.na(.x), yes = "&ndash;", no = .x)
        ),
        day = factor(
          x = day,
          levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
        )
      ) |>
      dplyr::arrange(day)

    # Exit
    return(
      list(
        meta = dplyr::tibble(
          id = unique(case$pid),
          delta_days = length(unique(case$day[stringr::str_detect(string = case$delta, pattern = "</span>")])),
          total_days = length(unique(case$day))
        ),
        data = case,
        log = r |>
          dplyr::filter(summary == "Sleeping hours", participant_id == id) |>
          dplyr::pull(sleep_episode_log)
      )
    )
  }

  # Import SAS results
  sas <- haven::read_sas(config::get("cycle7_site2_sas_results")) |>
    dplyr::rename_with(tolower)

  # Get participant meta
  meta <- haven::read_sas(config::get("cycle7_clinic_file")) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(mec_visit_date = paste0(
      v2_year,
      "-",
      stringr::str_pad(string = v2_mth, width = 2, pad = "0"),
      "-",
      stringr::str_pad(string = v2_day, width = 2, pad = "0")
    ) |>
      as.Date(),
    start_date = mec_visit_date + 1
    ) |>
    dplyr::select(clinicid, clc_age, site, start_date) |>
    dplyr::rename(id = 1, age = 2) |>
    dplyr::filter(site == 2) |>
    dplyr::mutate(
      agd_lfe = paste0(config::get("cycle7_site2_agd_dir_lfe"), "/", id, "15sec.agd"),
      agd_nml = paste0(config::get("cycle7_site2_agd_dir_nml"), "/", id, "15sec.agd"),
      epoch_length = dplyr::case_when(
        age >= 18 ~ 60,
        age >= 3 ~ 15,
        TRUE ~ NA
      ),
      dplyr::across(
        .cols = agd_lfe:agd_nml,
        .fns = ~ ifelse(test = file.exists(.x), yes = .x, no = NA)
      )
    )

  # Initialize agd R6 class
  agd_data <- agd$new(
    id = meta$id,
    age = meta$age,
    agd_lfe = meta$agd_lfe,
    agd_nml = meta$agd_nml,
    epoch_length = meta$epoch_length,
    day_max = 7,
    sleep_algo = "barreira",
    non_wear_algo = "barreira",
    start_date = meta$start_date,
    cpu_max = 15,
    dir = getwd()
  )

  # Run processing pipeline (load, clean, classify and summarize data)
  agd_data$run()

  # Get R results
  r <- agd_data$results$summary_sleeping_hours

  # Compare SAS and R sleep results
  sas_vs_r <- dplyr::bind_rows(
    lapply(
      X = unique(sas$pid),
      FUN = function(x) compare_sleep(id = x, tolerance = 4)$meta
    )
  )

  # Run basic checks
  testthat::expect_lte(
    object = sum(sas_vs_r$delta_days) / sum(sas_vs_r$total_days),
    expected = 5.00
  )
})
