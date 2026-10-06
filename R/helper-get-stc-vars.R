#' @noRd
get_stc_vars <- function(stems) {
  # Exit
  lapply(
    X = paste0("AMGD", stems),
    FUN = function(x) c(paste0(x, 1:7), x)
  ) |>
    unlist()
}
