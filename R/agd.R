#' @title R6 class: `agd`
#' @description `agd` is an R6 class that runs a data processing pipeline on
#' one or more jobs that include two `.agd` (ActiGraph; github.com/actigraph)
#' accelerometer files with `LowFrequencyExtension` and `Normal` filters per
#' participant. This class makes calls to the [agd_worker] R6 class.
#' @examples
#' # Create meta data frame (external/non-statcan users)
#' meta <- data.frame(
#'   id = c("jane-canuck", "john-canuck"),
#'   age = c(10, 40),
#'   agd_lfe = c(
#'     system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
#'     system.file("extdata", "john-canuck-lfe.agd", package = "chms")
#'   ),
#'   agd_nml = c(
#'     system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
#'     system.file("extdata", "john-canuck-nml.agd", package = "chms")
#'   ),
#'   start_date = c("2021-05-30", "2021-05-27"),
#'   epoch_length = c(15, 60)
#' )
#'
#' # Initialize agd R6 class
#' agd_data <- agd$new(
#'   id = meta$id,
#'   age = meta$age,
#'   agd_lfe = meta$agd_lfe,
#'   agd_nml = meta$agd_nml,
#'   epoch_length = meta$epoch_length,
#'   day_max = 2,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira",
#'   start_date = meta$start_date,
#'   cpu_max = 1
#' )
#'
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' agd_data$run()
#' @export

agd <- R6::R6Class(
  classname = "agd",
  public = list(
    #' @field args A list of arguments passed into `agd$new()`. See
    #' documentation for `agd$new()` for details.
    args = list(),

    #' @field log A tibble with `method`, `timestamp`, `status`, and `message`
    #' vectors providing a record of events during the pipeline run.
    log = list(),

    #' @field jobs A tibble with vectors based on the `args` passed into
    #' `agd$new()` representing jobs to be run that are distributed across the
    #' number of CPUs set by `cpu_max` in `agd$new()`.
    jobs = dplyr::tibble(),

    #' @field results A list of five tibbles (`summary_full`,
    #' `summary_full_stc`, `summary_run`, `summary_sleeping_hours`,
    #' `summary_waking_hours`) summarizing pipeline run(s).
    results = list(),

    #' @description This method creates an instance of an `agd_worker` class.
    #' @param id Required: a vector representing unique participant ID(s).
    #' @param age Required: an integer vector representing participant age(s)
    #' in years.
    #' @param agd_lfe Required: a character vector representing the full path
    #' to `.agd` file(s) with the `LowFrequencyExtension` filter.
    #' @param agd_nml Required: a character vector representing the full path
    #' to `.agd` file(s) with the `Normal` filter.
    #' @param epoch_length Required (default: `60`): an integer vector
    #' (length-one or the same length as `id`) representing the epoch length(s)
    #' at which to process the data. Statistics Canada currently uses `15` for
    #' participants under 18 years and `60` for participants at all other ages.
    #' Note: if `sleep_algo` is set to `"barreira"` and `epoch_length` is set
    #' to `15`, the sleep algorithm will be applied to 60-second epoch data and
    #' the results will be applied to the 15-second epoch data.
    #' @param day_max Required (default: `7`): an integer vector (length-one or
    #' the same length as `id`) representing the maximum number of days of data
    #' to load from `agd_lfe` and `agd_nml`.
    #' @param sleep_algo Required (default: `"barreira"`): a character vector
    #' (length-one or the same length as `id`) representing the sleep algorithm
    #' to apply. Options currently include `"barreira"`. See
    #' [apply_barreira_algo()] for more details.
    #' @param non_wear_algo Required (default: `"barreira"`): a character
    #' vector (length-one or the same length as `id`) representing the non-wear
    #' algorithm to apply. Options currently include `"barreira"`,
    #' `"20-min-algo"`, `"60-min-algo"`, `"90-min-algo"` and `"choi"`. See
    #' [apply_barreira_algo()] and [apply_non_wear_algo()] for more details.
    #' @param start_date Optional (default: `NA`): a character or date vector
    #' (format: yyyy-mm-dd) that is length-one or the same length as `id`
    #' representing the first day of data to load from `agd_lfe` and `agd_nml`.
    #' If not set, data will be loaded from the first available day until
    #' `day_max` is reached.
    #' @param cpu_max Required (default: `1`): a length-one integer vector
    #' representing the number of CPUs to distribute the data processing across.
    #' @param dir Optional (default: `NA`): a length-one character vector
    #' representing the full path to the location where the `results` list will
    #' be exported tibble by tibble in `.csv` format.
    #' @return Returns an object of class `agd`.

    initialize = function(
      id,
      age,
      agd_lfe,
      agd_nml,
      epoch_length = 60,
      day_max = 7,
      sleep_algo = "barreira",
      non_wear_algo = "barreira",
      start_date = NA,
      cpu_max = 1,
      dir = NA
    ) {
      # Update log
      private$update_log("new()")

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
        start_date = start_date,
        cpu_max = cpu_max,
        dir = dir
      ) |>
        dplyr::mutate(
          id = as.character(id),
          dir = dplyr::case_when(
            is.na(dir) ~ "",
            TRUE ~ dir
          )
        ) |>
        as.list()

      # Truncate select arguments
      self$args <- lapply(
        X = setNames(nm = names(self$args)),
        FUN = function(x) {
          if (x %in% c("cpu_max", "dir")) {
            self$args[[x]][1]
          } else {
            self$args[[x]]
          }
        }
      )

      # Coerce self$args$start_date to as.Date
      if (inherits(self$args$start_date, "character")) {
        self$args$start_date <- suppressWarnings(as.Date(self$args$start_date))
      }

      # Throw error if suggested packages are not installed but needed
      if (
        sum("choi" %in% self$args$non_wear_algo) &&
          ! rlang::is_installed("PhysicalActivity")
      ) {
        cli::cli_abort(
          paste(
            "To use the Choi non-wear algorithm, please install",
            "the {.pkg PhysicalActivity} package."
          )
        )
      }

      # Create jobs
      self$jobs <- self$args |>
        dplyr::as_tibble() |>
        dplyr::select(-cpu_max, -dir)

      # Update cpu_max if greater than jobs or available CPUs
      self$args$cpu_max <- min(
        self$args$cpu_max, nrow(self$jobs),
        parallelly::availableCores()
      )

      # Exit
      invisible(self)
    },

    #' @description This method iterates `jobs` and calls [run_agd_job()].
    #' @return Returns the `results` list from an [agd_worker] object and binds
    #' it to `self` (an instance of `agd`).

    run = function() {
      # Bind data
      tryCatch(
        expr = {
          # Render message to console
          cli::cli_h2(
            paste0(
              private$red("\U1F341"),
              private$black("{.emph chms::agd$run()} method")
            )
          )
          cli::cli_alert_info(
            paste(
              "Crunching data for {nrow(self$jobs)} participant{?s} across",
              "{self$args$cpu_max} CPU{?s}."
            )
          )
          cli::cli_text("")

          # Iterate jobs
          self$results <- Reduce(
            function(x, y) Map(dplyr::bind_rows, x, y),
            if (self$args$cpu_max > 1) {
              private$run_in_parallel(
                jobs = self$jobs,
                cpu_max = self$args$cpu_max
              )
            } else {
              lapply(
                X = cli::cli_progress_along(
                  seq_len(nrow(self$jobs)), "Progress"
                ),
                FUN = function(x) run_agd_job(self$jobs[x, ])
              )
            }
          )
        },
        error = function(e) e
      )

      # If no error in results
      if (! inherits(self$results, "error")) {
        # If self$args$dir exists
        if (dir.exists(self$args$dir)) {
          # Set download directory
          dir <- paste0(
            self$args$dir,
            "/agd-run-",
            gsub(pattern = " |[:]|[.]", replacement = "-", x = Sys.time())
          )

          # Create directory
          dir.create(path = dir, showWarnings = FALSE)

          # Render message to console
          cli::cli_alert_info("Exporting results to {.file {dir}}.")

          # Iterate self$results items
          out <- sapply(
            X = seq_along(self$results),
            FUN = function(x) {
              # Write to .csv
              readr::write_csv(
                x = self$results[[x]],
                file = paste0(
                  dir,
                  "/",
                  gsub(
                    pattern = "_",
                    replacement = "-",
                    x = names(self$results)[x]
                  ),
                  ".csv"
                ),
                na = ""
              )

              # Exit
              invisible()
            }
          )
        } else {
          # If dir argument is set
          if (self$args$dir != "") {
            # Render message to console
            cli::cli_alert_warning(
              paste(
                "Results cannot be exported because {.file {self$args$dir}}",
                "does not exist. Please set {.var dir} in the {.fn $export}",
                "call to save the results to file."
              )
            )
          }
        }
      }

      # Update log
      private$update_log("run()")

      # Render message to console
      if (self$args$dir != "") cli::cli_text("")
      cli::cli_text(paste0(cli::col_green("\u2714"), " Done!"))

      # Exit
      invisible(self)
    },

    #' @description This method exports `self$results` tibble by tibble in
    #' `.csv` format.
    #' @param dir Optional (default: `self$args$dir`): a length-one character
    #' vector representing the full path to the location where the `results`
    #' list will be exported tibble by tibble in `.csv` format.
    #' @param stc Optional (default: `FALSE`): a length-one logical vector
    #' indicating whether to export only statcan-formatted results.
    #' @return Returns the `agd` object (`self`) invisibly.

    export = function(dir = self$args$dir, stc = FALSE) {
      # Render message to console
      cli::cli_h2(
        paste0(
          private$red("\U1F341"),
          private$black("{.emph chms::agd$export()} method")
        )
      )

      # If no error in results
      if (! inherits(self$results, "error")) {
        # If self$args$dir exists
        if (dir.exists(dir)) {
          # If exporting all results
          if (isFALSE(stc)) {
            # Set download directory
            dir <- paste0(
              dir,
              "/agd-run-",
              gsub(pattern = " |[:]|[.]", replacement = "-", x = Sys.time())
            )

            # Create directory
            dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)

            # Get tibbles to export
            tibble_names <- names(self$results)

            # Render message to console
            cli::cli_alert_info("Exporting results to {.file {dir}}.")
          } else {
            tibble_names <- c("summary_full_stc", "summary_run")

            # Render message to console
            cli::cli_alert_info(
              paste(
                "Exporting {.var self$results$summary_full_stc} and {.var",
                "self$results$summary_run} to {.file {dir}}."
              )
            )
          }

          # Iterate self$results items
          out <- sapply(
            X = tibble_names,
            FUN = function(x) {
              # Write to .csv
              readr::write_csv(
                x = self$results[[x]],
                file = paste0(
                  dir,
                  "/",
                  gsub(pattern = "_", replacement = "-", x = x),
                  ".csv"
                ),
                na = ""
              )

              # Exit
              invisible()
            }
          )

          # Render message to console
          cli::cli_text("")
          cli::cli_alert_success("Done!")
        } else {
          # Render message to console
          if (dir == "") {
            cli::cli_abort(
              paste(
                "Results cannot be exported because {.var dir} is not set.",
                "Please set {.var dir} in the {.fn $export} call to save the",
                "results to file."
              )
            )
          } else {
            cli::cli_abort(
              paste(
                "Results cannot be exported because {.file {dir}} does not",
                "exist. Please set {.var dir} in the {.fn $export} call to",
                "save the results to file."
              )
            )
          }
        }
      } else {
        # Render message to console
        cli::cli_abort(
          "There are no results to export. Call {.fn $run} to crunch data."
        )
      }

      # Exit
      invisible(self)
    },

    #' @description This method renders details (arguments, issues, log) about
    #' an instance `agd` to the console.
    #' @return Returns the `agd` object (`self`) invisibly.

    print = function() {
      # Render agd/agd_worker details to console
      if (nrow(self$jobs) > 1) {
        # Render messages to console
        cli::cli_h2(
          paste0(
            private$red("\U1F341"),
            private$black("{.emph chms::agd$print()} method")
          )
        )
        cli::cli_text("{.strong Settings}")
        cli::cli_text("")

        # Print args to console
        self$jobs |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = ~ as.character(.x)
            ),
            dplyr::across(
              .cols = agd_nml:agd_lfe,
              .fns = ~ stringr::str_trunc(
                string = .x,
                width = 24,
                side = "center"
              )
            ),
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = ~ tidyr::replace_na(data = .x, replace = "")
            )
          ) |>
          print()

        # Render message to console
        cli::cli_text("")
        cli::cli_text("{.strong Log}")
        cli::cli_text("")

        # Print log to console
        self$log |>
          print(n = nrow(self$log))
      } else {
        # Render messages to console
        cli::cli_h2(
          paste0(
            private$red("\U1F341"),
            private$black("{.emph chms::agd} class")
          )
        )
        cli::cli_text("{.strong Settings}")
        cli::cli_text("")

        # Reshape args
        if ("run()" %in% self$log$method) {
          args <- self$results$summary_run$args |>
            jsonlite::fromJSON() |>
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
            length(jsonlite::fromJSON(self$results$summary_run$device_settings))
          ) {
            args <- dplyr::bind_rows(
              args,
              self$results$summary_run$device_settings |>
                jsonlite::fromJSON() |>
                dplyr::select(-sex, -height, -mass, -age, -dateOfBirth) |>
                tidyr::pivot_longer(cols = dplyr::everything())
            )
          }
        } else {
          args <- self$args |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              dplyr::across(
                .cols = dplyr::everything(),
                .fns = ~ as.character(.x)
              )
            ) |>
            tidyr::pivot_longer(cols = dplyr::everything())
        }

        # Print args to console
        args |>
          dplyr::mutate(
            value = tidyr::replace_na(data = value, replace = "")
          ) |>
          print(n = nrow(args))

        # Render issues to console
        if ("run()" %in% self$log$method) {
          # Render message to console
          cli::cli_text("")
          cli::cli_text("{.strong Issues}")
          cli::cli_text("")

          self$results$summary_run$issues |>
            jsonlite::fromJSON() |>
            utils::stack() |>
            dplyr::as_tibble() |>
            dplyr::relocate(ind) |>
            dplyr::rename(name = ind, value = values) |>
            dplyr::mutate(name = as.character(name)) |>
            print()
        }

        # Render message to console
        cli::cli_text("")
        cli::cli_text("{.strong Log}")
        cli::cli_text("")

        # Render log to console
        if ("run()" %in% self$log$method) {
          self$results$summary_run$log |>
            jsonlite::fromJSON() |>
            dplyr::as_tibble() |>
            print()
        } else {
          self$log |>
            dplyr::as_tibble() |>
            print()
        }
      }

      # Exit
      invisible(self)
    },

    #' @description This method renders a sanity check report in .html format.
    #' @param name Required (default: `"sanity-check-report"`): a length-one
    #' character vector representing the file name of the report.
    #' @param id Required (default: `self$jobs$id`): a vector representing
    #' unique participant ID(s).
    #' @param dir Required (default: `self$args$dir`): a length-one character
    #' vector representing the full path to the location where the report will
    #' be exported in `.html` format.
    #' @param include_plot Optional (default: `FALSE`): a length-one logical
    #' vector representing whether to render scatterplots.
    #' @return Returns an .html-formatted report.

    sanity_check = function(
      name = "sanity-check-report",
      id = self$jobs$id,
      dir = self$args$dir,
      include_plot = FALSE
    ) {
      # Render messages to console
      cli::cli_h2(
        paste0(
          private$red("\U1F341"),
          private$black("{.emph chms::sanity_check()} method")
        )
      )

      # Throw error is dir does not exist
      if (! dir.exists(dir)) {
        if (dir == "") {
          cli::cli_abort(
            paste(
              "The {.var dir} argument is not set. Please set {.var dir}",
              "in the {.fn $sanity_check} call."
            )
          )
        } else {
          cli::cli_abort(paste(
            "The {.file {dir}} path does not exist. Please set {.var dir}",
            "in the {.fn $sanity_check} call."
          ))
        }
      }

      # Throw error if suggested packages are not installed but needed
      if (! rlang::is_installed("ggplot2")) {
        cli::cli_abort("Please install the {.pkg ggplot2} package.")
      } else if (! rlang::is_installed("kableExtra")) {
        cli::cli_abort("Please install the {.pkg kableExtra} package.")
      } else if (! rlang::is_installed("quarto")) {
        cli::cli_abort("Please install the {.pkg quarto} package.")
      } else if (! rlang::is_installed("scales")) {
        cli::cli_abort("Please install the {.pkg scales} package.")
      } else if (! rlang::is_installed("tibble")) {
        cli::cli_abort("Please install the {.pkg tibble} package.")
      }

      # Set report name
      report_name_qmd <- paste0(name, ".qmd")
      report_name_html <- paste0(name, ".html")

      # Copy quarto doc to dir
      copy_file <- file.copy(
        from = system.file("qmd", "sanity-check-report.qmd", package = "chms"),
        to = paste0(dir, "/", report_name_qmd),
        overwrite = TRUE
      )

      # Render message to console
      cli::cli_text("Rendering sanity check report in .html format.")

      # Temporarily save parameters to temp directory
      timestamp <- gsub(" |[:]|[.]|-", "_", Sys.time())
      dir.create(
        path = paste0(dir, "/agd-temp/sanity-check-params/"),
        showWarnings = FALSE,
        recursive = TRUE
      )
      saveRDS(
        object = self$results$summary_run,
        file = paste0(dir, "/agd-temp/sanity-check-params/summary-run.rds")
      )
      saveRDS(
        object = self$results$summary_waking_hours,
        file = paste0(
          dir,
          "/agd-temp/sanity-check-params/summary-waking-hours.rds"
        )
      )
      saveRDS(
        object = self$results$summary_sleeping_hours,
        file = paste0(
          dir,
          "/agd-temp/sanity-check-params/summary-sleeping-hours.rds"
        )
      )

      # Render Quarto doc
      quarto::quarto_render(
        input = paste0(
          dir,
          "/",
          report_name_qmd
        ),
        execute_params = list(
          summary_run = paste0(
            dir,
            "/agd-temp/sanity-check-params/summary-run.rds"
          ),
          summary_waking_hours = paste0(
            dir,
            "/agd-temp/sanity-check-params/summary-waking-hours.rds"
          ),
          summary_sleeping_hours = paste0(
            dir,
            "/agd-temp/sanity-check-params/summary-sleeping-hours.rds"
          ),
          include_plot = include_plot
        )
      )

      # Clean up
      unlink(
        x = paste0(dir, c("/agd-temp", paste0("/", report_name_qmd))),
        recursive = TRUE
      )

      # Render message to console
      cli::cli_text(paste0(cli::col_green("\u2714"), " Done!"))
      cli::cli_text("")
      cli::cli_text(
        paste(
          "The sanity check report is available here: {.url",
          "{paste0(dir, '/', report_name_html)}}"
        )
      )
      cli::cli_text("")

      # Send report to browser
      utils::browseURL(url = paste0(dir, "/", report_name_html))

      # Exit
      invisible(self)
    },

    #' @description This method renders data frames from `$results` in tab.
    #' @param results Optional (default: `c(names(self$results), "issues",
    #' "log")`): a character vector representing the results to render to tab.
    #' Options include: `"summary_full"`,
    #' `"summary_full_stc"`, `"summary_run"`, `"summary_sleeping_hours"`,
    #' `"summary_waking_hours"`, `"issues"`, `"log"`.
    #' @return Returns the `agd` object (`self`) invisibly.

    view = function(results = c(names(self$results), "issues", "log")) {
      # Render message to console
      cli::cli_h2(
        paste0(
          private$red("\U1F341"),
          private$black("{.emph chms::view()} method")
        )
      )
      cli::cli_alert_info(
        paste0(
          "Attempting to open tabs for the following results:
          ", paste0(results, collapse = ", "),
          "."
        )
      )
      cli::cli_text("")

      if (is.null(names(self$results)) || ! length(results)) {
        # Render console message
        cli::cli_abort("There are no results to view.")
      } else {
        # Iterate results and call View()
        for (result in results) {
          if (result == "issues") {
            dplyr::bind_cols(
              self$results$summary_run |> dplyr::select(participant_id),
              dplyr::bind_rows(
                lapply(
                  X = self$results$summary_run[[result]],
                  FUN = jsonlite::fromJSON
                )
              )
            ) |>
              View(title = result)
          } else if (result == "log") {
            dplyr::bind_rows(
              lapply(
                X = self$results$summary_run[[result]],
                FUN = jsonlite::fromJSON
              )
            ) |>
              View(title = result)
          } else {
            View(self$results[[result]], title = result)
          }
        }
      }

      # Render console message
      cli::cli_alert_success("Done!")

      # Exit
      invisible(self)
    }
  ),
  private = list(
    black = function(x) cli::make_ansi_style("#000000")(x),
    red = function(x) cli::make_ansi_style("#af3c43")(x),
    run_in_parallel = function(jobs, cpu_max) {
      # Start workers
      mirai::daemons(cpu_max)

      # Load chms on all daemons
      mirai::everywhere(library(chms))

      # Iterate jobs
      results <- mirai::mirai_map(
        .x = seq_len(nrow(jobs)),
        .f = function(x, jobs) chms::run_agd_job(jobs[x, ]),
        .args = list(jobs = jobs)
      )[.progress]

      # Stop workers
      mirai::daemons(0)

      # Exit
      results
    },
    update_log = function(method) {
      self$log <- dplyr::bind_rows(
        self$log,
        dplyr::tibble(
          method = method,
          timestamp = Sys.time(),
          status = ifelse(
            test = "error" %in% class(self),
            yes = "failure",
            no = "success"
          ),
          message = ifelse(
            test = "error" %in% class(self),
            yes = gsub(
              pattern = '"',
              replacement = "'",
              x = paste0(self$message)
            ),
            no = ""
          )
        )
      )
    }
  )
)
