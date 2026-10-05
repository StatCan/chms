#' @title R6 class: `agd_worker`
#' @description `agd_worker` is an R6 class that runs a data processing
#' pipeline on two `.agd` (ActiGraph; github.com/actigraph) accelerometer files
#' with `LowFrequencyExtension` and `Normal` filters for a single participant.
#' This class does all the heavy lifting for the [agd] R6 class.
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
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' agd_data$run()
#' @export

agd_worker <- R6::R6Class(
  classname = "agd_worker",
  public = list(
    #' @field args A list of arguments passed into `agd_worker$new()`. See
    #' documentation for `agd_worker$new()` for details.
    args = list(),

    #' @field data A list of four tibbles created during `agd_worker$load()`
    #' (`settings`, `raw`), `agd_worker$clean()` (`clean`) and
    #' `agd_worker$classify()` (`classify`) representing `.agd` data at various
    #' stages of the pipeline.
    data = list(),

    #' @field issues A list of seven length-one, dichotomous (`"yes"`/`""`)
    #' character vectors (`file_missing`, `files_identical`, `files_mismatched`,
    #' `non_midnight_start`, `no_complete_days`, `sleep_missing`,
    #' `age_out_of_range`) representing flags for issues affecting pipeline
    #' results.
    issues = list(
      file_missing = "",
      file_empty = "",
      files_identical = "",
      files_mismatched = "",
      non_midnight_start = "",
      no_complete_days = "",
      sleep_missing = "",
      age_out_of_range = ""
    ),

    #' @field log A tibble with `method`, `timestamp`, `status` and `message`
    #' vectors providing a record of events during the pipeline run.
    log = dplyr::tibble(),

    #' @field results A list of four tibbles (`summary_full`,
    #' `summary_full_stc`, `summary_sleeping_hours`, `summary_waking_hours`)
    #' summarizing the pipeline run.
    results = list(),

    #' @description This method creates an instance of an `agd_worker` class.
    #' @param id Required: a length-one vector representing a unique
    #' participant ID.
    #' @param age Required: a length-one integer vector representing a
    #' participant's age in years.
    #' @param agd_lfe Required: a length-one character vector representing the
    #' full path to an `.agd` file with the `LowFrequencyExtension` filter.
    #' @param agd_nml Required: a length-one character vector representing the
    #' full path to an `.agd` file with the `Normal` filter.
    #' @param epoch_length Required (default: `60`): a length-one integer
    #' vector representing the epoch length at which to process the data.
    #' Statistics Canada currently uses `15` for participants under 18 years
    #' and `60` for participants at all other ages. Note: if `sleep_algo` is
    #' set to `"barreira"` and `epoch_length` is set to `15`, the sleep
    #' algorithm will be applied to 60-second epoch data and the results will
    #' be applied to the 15-second epoch data.
    #' @param day_max Required (default: `7`): a length-one integer vector
    #' representing the maximum number of days of data to load from `agd_lfe`
    #' and `agd_nml`.
    #' @param sleep_algo Required (default: `"barreira"`): a length-one
    #' character vector representing the sleep algorithm to apply. Options
    #' currently include `"barreira"`. See [apply_barreira_algo()] for more
    #' details.
    #' @param non_wear_algo Required (default: `"barreira"`): a length-one
    #' character vector representing the non-wear algorithm to apply. Options
    #' currently include `"barreira"`, `"20-min-algo"`, `"60-min-algo"`,
    #' `"90-min-algo"` and `"choi"`. See [apply_barreira_algo()] and
    #' [apply_non_wear_algo()] for more details.
    #' @param start_date Optional (default: `NA`): a length-one date vector
    #' (format: yyyy-mm-dd) representing the first day of data to load from
    #' `agd_lfe` and `agd_nml`. If not set, data will be loaded from the first
    #' available day until `day_max` is reached.
    #' @return Returns an object of class `agd_worker`.

    initialize = function(
      id,
      age,
      agd_lfe,
      agd_nml,
      epoch_length = 60,
      day_max = 7,
      sleep_algo = "barreira",
      non_wear_algo = "barreira",
      start_date = NA
    ) {
      # Update log
      self$log <- dplyr::tibble(
        method = "new()",
        timestamp = Sys.time(),
        status = "success",
        message = ""
      )

      # Bind args
      self$args <- dplyr::tibble(
        id = id,
        age = age,
        agd_lfe = agd_lfe,
        agd_nml = agd_nml,
        epoch_length = epoch_length,
        day_max = day_max,
        sleep_algo = sleep_algo,
        non_wear_algo = non_wear_algo,
        start_date = start_date
      ) |>
        dplyr::mutate(id = as.character(id)) |>
        as.list()

      # Update issues
      if (self$args$age < 3) self$issues$age_out_of_range <- "yes"

      # Exit
      invisible(self)
    },

    #' @description This method calls [load_agd_settings()] and
    #' [load_agd_data()].
    #' @return Returns the `agd_worker` object (`self`) invisibly.

    load = function() {
      # Bind .agd settings from agd_lfe
      self$data$settings <- tryCatch(
        expr = load_agd_settings(self$args$agd_lfe),
        error = function(e) e
      )

      # Get settings for agd_nml
      settings_agd_nml <- tryCatch(
        expr = load_agd_settings(self$args$agd_nml),
        error = function(e) e
      )

      # Set vector names from settings for downstream comparison
      vectors_to_compare <- c(
        "deviceserial",
        "startdatetime",
        "epochlength",
        "filter"
      )

      # Bind .agd data
      self$data$raw <- tryCatch(
        expr = dplyr::left_join(
          x = load_agd_data(
            file = self$args$agd_lfe,
            day_max = self$args$day_max,
            start_date = self$args$start_date,
            settings = self$data$settings
          ) |>
            dplyr::rename(steps_lfe = steps),
          y = load_agd_data(
            file = self$args$agd_nml,
            col_select = c("dataTimestamp", "steps"),
            day_max = self$args$day_max,
            start_date = self$args$start_date,
            settings = settings_agd_nml
          ) |>
            dplyr::select(dataTimestamp, steps) |>
            dplyr::rename(steps_normal = steps),
          by = "dataTimestamp"
        ) |>
          dplyr::relocate(steps_normal, .before = steps_lfe),
        error = function(e) e
      )

      # Update issues
      if ("error" %in% c(class(self$data$settings), class(settings_agd_nml))) {
        self$issues$file_missing <- "yes"
      } else if (
        isTRUE(
          all.equal(
            self$data$settings[vectors_to_compare],
            settings_agd_nml[vectors_to_compare]
          )
        )
      ) {
        self$issues$files_identical <- "yes"
      } else if (
        ! isTRUE(
          all.equal(
            self$data$settings[vectors_to_compare[1:3]],
            settings_agd_nml[vectors_to_compare[1:3]]
          )
        )
      ) {
        self$issues$files_mismatched <- "yes"
      }

      if (! inherits(self$data$raw, "error") && ! nrow(self$data$raw)) {
        self$issues$file_empty <- "yes"
      }

      if (
        ! inherits(self$data$settings, "error") &&
          format(
            x = as.POSIXct(
              x = self$data$settings$startdatetime,
              tz = self$data$settings$time_zone
            ),
            format = "%H"
          ) != "00"
      ) {
        self$issues$non_midnight_start <- "yes"
      }

      # Update log
      self$log <- dplyr::bind_rows(
        self$log,
        dplyr::tibble(
          method = "load()",
          timestamp = Sys.time(),
          status = ifelse(
            test = "error" %in% c(
              class(self$data$settings),
              class(self$data$raw),
              class(settings_agd_nml)
            ),
            yes = "failure",
            no = "success"
          ),
          message = ifelse(
            test = "error" %in% c(
              class(self$data$settings),
              class(self$data$raw),
              class(settings_agd_nml)
            ),
            yes = suppressWarnings(
              gsub(
                pattern = '"',
                replacement = "'",
                x = paste0(
                  self$data$settings$message,
                  self$data$raw$message,
                  settings_agd_nml$message,
                  collapse = "; "
                )
              )
            ),
            no = ""
          )
        )
      )

      # Exit
      invisible(self)
    },

    #' @description This method calls [clean_agd_data()].
    #' @return Returns the `agd_worker` object (`self`) invisibly.

    clean = function() {
      # Clean data
      tryCatch(
        expr = clean_agd_data(self),
        error = function(e) e
      )

      # Update log
      private$update_log("clean()")

      # Exit
      invisible(self)
    },

    #' @description This method calls [classify_agd_data()].
    #' @return Returns the `agd_worker` object (`self`) invisibly.

    classify = function() {
      # classify data
      tryCatch(
        expr = classify_agd_data(self),
        error = function(e) e
      )

      # Update log
      private$update_log("classify()")

      # Exit
      invisible(self)
    },

    #' @description This method calls [summarize_agd_data()].
    #' @return Returns the `agd_worker` object (`self`) invisibly.

    summarize = function() {
      # Summarize data
      tryCatch(
        expr = summarize_agd_data(self),
        error = function(e) e
      )

      # Update log
      private$update_log("summarize()")

      # Exit
      invisible(self)
    },

    #' @description This method calls `agd_worker$load()`,
    #' `agd_worker$clean()`, `agd_worker$classify()` and
    #' `agd_worker$summarize()`.
    #' @return Returns the `agd_worker` object (`self`) invisibly.

    run = function() {
      # Run all methods on data
      tryCatch(
        expr = self$load()$clean()$classify()$summarize(),
        error = function(e) e
      )

      # If run() failed, add empty data frames to x$results
      if (! length(self$results)) {
        self$results <- list(
          summary_full = dplyr::tibble(),
          summary_full_stc = dplyr::tibble(),
          summary_waking_hours = dplyr::tibble(),
          summary_sleeping_hours = dplyr::tibble()
        )
      }

      # Exit
      invisible(self)
    },

    #' @description This method renders details (arguments, issues, log) about
    #' an instance of `agd_worker` to the console.
    #' @return Returns the `agd_worker` object (`self`) invisibly.

    print = function() {
      # Render messages to console
      cli::cli_h2(
        paste0(
          private$red("\U1F341"),
          private$black("{.emph agd_worker} class")
        )
      )
      cli::cli_text("{.strong Settings}")
      cli::cli_text("")

      # Reshape args
      args <- self$args |>
        dplyr::as_tibble() |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::everything(),
            .fns = ~ as.character(.x)
          )
        ) |>
        tidyr::pivot_longer(cols = dplyr::everything())

      # Add to args if settings loaded
      if (
        "settings" %in% names(self$data) &&
          ! inherits(self$data$settings, "error")
      ) {
        args <- dplyr::bind_rows(
          args,
          self$data$settings |>
            dplyr::select(-sex, -height, -mass, -age, -dateOfBirth) |>
            tidyr::pivot_longer(cols = dplyr::everything())
        )
      }

      # Print args to console
      args |>
        dplyr::mutate(value = tidyr::replace_na(data = value, replace = "")) |>
        print(n = nrow(args))

      # Render message to console
      cli::cli_text("")
      cli::cli_text("{.strong Issues}")
      cli::cli_text("")

      # Render issues to console
      self$issues |>
        utils::stack() |>
        dplyr::as_tibble() |>
        dplyr::relocate(ind) |>
        dplyr::rename(name = ind, value = values) |>
        dplyr::mutate(name = as.character(name)) |>
        print()

      # Render message to console
      cli::cli_text("")
      cli::cli_text("{.strong Log}")
      cli::cli_text("")

      # Render log to console
      if ("summarize()" %in% self$log$method) {
        self$log |>
          dplyr::mutate(
            run_time = dplyr::case_when(
              dplyr::row_number() == dplyr::n() ~ as.numeric(
                difftime(max(timestamp), min(timestamp))
              ),
              TRUE ~ NA
            )
          ) |>
          print(n = nrow(self$log))
      } else {
        self$log |>
          print(n = nrow(self$log))
      }

      # Exit
      invisible(self)
    }
  ),
  private = list(
    black = function(x) cli::make_ansi_style("#000000")(x),
    red = function(x) cli::make_ansi_style("#af3c43")(x),
    update_log = function(method) {
      # Get data frame to check
      df <- gsub(pattern = "\\(|\\)", replacement = "", x = method)

      # Get list to check
      if (df == "summarize") {
        df <- "summary_full"
        dl <- "results"
      } else {
        dl <- "data"
      }

      # Check data frame for errors and set status, message
      if (! df %in% names(self[[dl]])) {
        status <- "failure"
        message <- ""
      } else if ("error" %in% class(self[[dl]][[df]])) {
        status <- "failure"
        message <- self[[dl]][[df]]$message
      } else {
        status <- "success"
        message <- ""
      }

      # Update issues
      if (
        df == "classify" &&
          status == "success" &&
          ! "SL" %in% self$data$classify$mvt_class
      ) self$issues$sleep_missing <- "yes"

      # Update log
      self$log <- dplyr::bind_rows(
        self$log,
        dplyr::tibble(
          method = method,
          timestamp = Sys.time(),
          status = status,
          message = message
        )
      )

      # Exit
      invisible(self)
    }
  )
)
