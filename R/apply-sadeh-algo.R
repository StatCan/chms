#' @title Classify an accelerometer axis vector of 60-second epochs as sleep time or awake time.
#' @description This function uses the Sadeh algorithm (https://pubmed.ncbi.nlm.nih.gov/7939118) to classify an accelerometer axis vector of 60-second epochs as sleep time or awake time.
#' @param x Required: a data frame of accelerometer data.
#' @param axis1 Required default (`"axis1"`): a length-one character vector representing the name of the vertical axis.
#' @param censor_counts Optional (default: `FALSE`): a length-one logical vector representing whether to censor accelerometer counts to a maximum of 300 before applying the Sadeh algorithm.
#' @param return Required (default: `"everything"`): a character vector representing which vectors to return. If set to "everything", the data frame in `x` will be returned along with all vectors that were derived while applying the Sadeh algorithm.
#' @return Returns `x` along with all vectors that were derived while applying the Sadeh algorithm. Note: values in the `sadeh_sleep_score` vector that are greater than -4 are interpreted as sleep time (see https://actigraphcorp.my.site.com/support/s/article/Where-can-I-find-documentation-for-the-Sadeh-and-Cole-Kripke-algorithms).
#' @export

apply_sadeh_algo <- function(x, axis1 = "axis1", censor_counts = FALSE, return = "everything") {
  # Rename vectors
  x <- x |> dplyr::rename(axis1 = dplyr::all_of(axis1))

  # Censor axis1 counts to a maximum of 300 if censor_counts argument set to TRUE
  if(isTRUE(censor_counts)) x <- x |>
      dplyr::mutate(axis1_original = axis1, axis1 = pmin(axis1_original, 300))

  # Compute Sadeh sleep score
  x <- x |>
    dplyr::mutate(
      # Compute a rolling average (11-epoch window width)
      # "AVG" in the Sadeh algorithm
      avg = zoo::rollapply(
        data = axis1,
        width = 11,
        FUN = mean,
        fill = NA,
        align = "center"
      ),

      # Compute a rolling sum (11-epoch window width) of counts that are at least 50 and less than 100
      # "NATS" in the Sadeh algorithm
      nats = zoo::rollapply(
        data = axis1,
        width = 11,
        FUN = function(x) sum(x >= 50 & x < 100),
        fill = NA,
        align = "center"
      ),

      # Compute a rolling standard deviation (6-epoch window width)
      # "SD" in the Sadeh algorithm
      sd = zoo::rollapply(
        data = axis1,
        width = 6,
        FUN = stats::sd,
        fill = NA,
        align = "right"
      ),

      # Compute sleep score
      sadeh_sleep_score = 7.601 - (0.065 * avg) - (1.08 * nats) - (0.056 * sd) - (0.703 * log(axis1 + 1))
    )

  # If axis1 counts were censored to a maximum of 300, restore original axis1 counts
  if(isTRUE(censor_counts)) x <- x |>
    dplyr::mutate(axis1 = axis1_original)

  # Restore original vector names
  x <- x |> dplyr::rename(!! axis1 := axis1)

  # Determine what to return and exit
  if("everything" %in% return) return(x) else return(x[return])
}
