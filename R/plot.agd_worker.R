#' @title Plot method for the [agd_worker] R6 class.
#' @description This method renders a scatter plot using results from an
#' [agd_worker] object.
#' @param x Required: an [agd_worker] object.
#' @param ... Optional: arguments to be passed to methods. **Note:** currently
#' not used.
#' @param title_size Required (default: `11`): a length-one numeric vector
#' representing the title font size.
#' @param axis_size Required (default: `9`): a length-one numeric vector
#' representing the axis font size .
#' @param label_size Required (default: `3`): a length-one numeric vector
#' representing the label font size.
#' @return Returns a `ggplot2` object invisibly.
#' @method plot agd_worker
#' @examples
#' # Initialize agd_worker R6 class
#' agd_data <- agd_worker$new(
#'   id = "jane-canuck",
#'   age = 10,
#'   agd_lfe = system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
#'   agd_nml = system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
#'   epoch_length = 15,
#'   day_max = 2,
#'   sleep_algo = "barreira",
#'   non_wear_algo = "barreira"
#' )
#'
#' # Run data processing pipeline (load, clean, classify and summarize data)
#' agd_data$run()
#'
#' # Plot data
#' plot(agd_data)
#' @export

plot.agd_worker <- function(
  x,
  ...,
  title_size = 11,
  axis_size = 9,
  label_size = 3
) {
  # Throw error if suggested packages are not installed but needed
  if (! rlang::is_installed("ggplot2")) {
    cli::cli_abort("Please install the {.pkg ggplot2} package.")
  } else if (! rlang::is_installed("scales")) {
    cli::cli_abort("Please install the {.pkg scales} package.")
  }

  # Create data for geom_rect and geom_text
  rect_data <- x$data$classify |>
    dplyr::group_by(ymd) |>
    dplyr::reframe(
      xmin = min(ymd_hm),
      xmax = max(ymd_hm),
      axis1 = max(axis1)
    ) |>
    dplyr::mutate(
      xmean = xmin + (60 * 60 * 12),
      axis1 = max(axis1),
      day_of_week = substr(weekdays(ymd), 1, 3),
      ymin = 0,
      ymax = Inf
    )

  # Create scatter plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = rect_data,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
      ),
      fill = rep(
        x = c("#ffffff", "#dddddd"),
        each = 1,
        length.out = nrow(rect_data)
      ),
      alpha = 0.35
    ) +
    ggplot2::geom_text(
      data = rect_data,
      ggplot2::aes(
        x = xmean,
        y = axis1,
        label = day_of_week
      ),
      size = label_size,
      hjust = 0.3
    ) +
    ggplot2::geom_point(
      data = x$data$classify |>
        dplyr::mutate(
          mvt_class = factor(
            x = mvt_class,
            levels = c(
              "NW",
              "SB",
              "LPA",
              "MPA",
              "VPA",
              "SL"
            )
          )
        ),
      mapping = ggplot2::aes(
        x = ymd_hm,
        y = axis1,
        color = mvt_class
      ),
      alpha = 0.75,
      size = 1
    ) +
    ggplot2::scale_x_datetime(
      breaks = "1 day",
      date_labels = "%b %d"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(
      values = c(
        "NW" = "#ef476f",
        "SB" = "#f78c6b",
        "LPA" = "#ffd166",
        "MPA" = "#06d6a0",
        "VPA" = "#118ab2",
        "SL" = "#073b4c"
      )
    ) +
    ggplot2::labs(
      title = paste0(
        "Processed ActiGraph data (LFE filtered) for participant ",
        x$args$id
      ),
      x = "",
      y = paste0(
        "Accelerometer counts (per ",
        x$args$epoch_length,
        " seconds)"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = title_size,
        margin = ggplot2::margin(t = 0, r = 0, b = 20, l = 0)
      ),
      axis.title.x = ggplot2::element_text(size = axis_size),
      axis.text.x = ggplot2::element_text(size = axis_size),
      axis.title.y = ggplot2::element_text(
        size = axis_size,
        margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)
      ),
      axis.text.y = ggplot2::element_text(size = axis_size),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = axis_size)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))

  # Render plot
  print(plot)

  # Exit
  invisible(plot)
}
