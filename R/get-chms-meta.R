#' @title Gets CHMS participant metadata.
#' @description This function gets CHMS participant metadata.
#' @param clinic_file Required: a length-one character vector representing the
#' full path to the clinic file. Note: a `.sas7bdat` file is expected.
#' @param agd_dir Required: a character vector representing the full path(s) to
#' the site directory ("full/path/to/data/site").
#' @param clinic_id Required (default: `"CLINICID"`): a length-one character
#' vector representing the clinic ID vector name in `clinic_file`.
#' @param site Required (default: `"SITE"`): a length-one character vector
#' representing the site vector name in `clinic_file`.
#' @param age Required (default: `"CLC_AGE"`): a length-one character vector
#' representing the age vector name in `clinic_file`.
#' @param day Required (default: `"V2_DAY"`): a length-one character vector
#' representing the day vector name of the MEC visit date in `clinic_file`.
#' @param month Required (default: `"V2_MTH"`): a length-one character vector
#' representing the month vector name of the MEC visit date in `clinic_file`.
#' @param year Required (default: `"V2_YEAR"`): a length-one character vector
#' representing the year vector name of the MEC visit date in `clinic_file`.
#' @return Returns a tibble with eight vectors (`id`, `age`, `site`, `agd_lfe`,
#' `agd_nml`, `mec_visit_date`, `start_date`, `epoch_length`).
#' @examples \donttest{
#' # Create participant meta (statcan users)
#' meta <- get_chms_meta(
#'   clinic_file = "path/to/clinic/file.sas7bdat",
#'   agd_dir = "path/to/agd/files/site",
#'   clinic_id = "CLINICID",
#'   site = "SITE",
#'   age = "CLC_AGE",
#'   day = "V2_DAY",
#'   month = "V2_MTH",
#'   year = "V2_YEAR"
#' )
#' }
#' @export

get_chms_meta <- function(
  clinic_file,
  agd_dir,
  clinic_id = "CLINICID",
  site = "SITE",
  age = "CLC_AGE",
  day = "V2_DAY",
  month = "V2_MTH",
  year = "V2_YEAR"
) {
  # Get participant meta
  meta <- haven::read_sas(
    data_file = clinic_file,
    col_select = dplyr::all_of(c(clinic_id, site, age, day, month, year))
  ) |>
    dplyr::rename(
      id = dplyr::all_of(clinic_id),
      site = dplyr::all_of(site),
      age = dplyr::all_of(age),
      day = dplyr::all_of(day),
      month = dplyr::all_of(month),
      year = dplyr::all_of(year),
    ) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::arrange(site, id) |>
    dplyr::mutate(
      agd_nml = paste0(
        agd_dir,
        stringr::str_pad(
          string = site,
          width = 2,
          pad = 0
        ),
        "/Normal_filter/",
        id,
        "15sec.agd"
      ),
      agd_lfe = gsub("Normal_", "LFE_", agd_nml),
      mec_visit_date = lubridate::ymd(
        paste0(
          year,
          "-",
          month,
          "-",
          day
        ),
        quiet = TRUE
      ),
      start_date = mec_visit_date + 1,
      epoch_length = ifelse(
        test = age < 18,
        yes = 15,
        no = 60
      )
    )

  # Remove unwanted vectors
  meta <- meta |>
    dplyr::select(-day, -month, -year) |>
    dplyr::relocate(agd_lfe, .before = agd_nml)

  # Exit
  meta
}
