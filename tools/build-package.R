# Create DESCRIPTION file
usethis::use_description(
  fields = list(
    Package = "chms",
    Version = "7.1",
    Title = "Accelerometer Processing Methods for Cycle 7 of the CHMS",
    Description = "'ActiGraph wGT3X-BT' accelerometer processing methods using the standardized workflow developed by Statistics Canada for cycle 7 of the Canadian Health Measures Survey (CHMS). The package promotes transparent and reproducible data processing while supporting the harmonization of analytical approaches among researchers wishing to align with Statistics Canada's methods. For general details about the processing methods, please consult Clarke J, Gribbon A, St-Laurent M, Ferrao T, Barnes J, Kuzik N, Colley R (2026) <doi: 10.25318/82-003-x202600200001-eng>.",
    `Authors@R` = c(
      utils::person(
        given = "Joel",
        family = "Barnes",
        email = "joel.barnes@statcan.gc.ca",
        role = c("aut", "cre")
      ),
      utils::person(
        given = "Janine",
        family = "Clarke",
        email = "janine.clarke@statcan.gc.ca",
        role = c("ctb")
      ),
      utils::person(
        given = "Rachel",
        family = "Colley",
        email = "rachel.colley@statcan.gc.ca",
        role = c("ctb")
      ),
      utils::person(
        given = "His Majesty the King in Right of Canada, as represented by Statistics Canada",
        role = "cph"
      )
    ),
    License = "MIT",
    URL = "https://github.com/statcan/chms",
    BugReports = "https://github.com/statcan/chms/issues",
    Depends = "R (>= 4.1.0)"
  )
)

# Add dependencies to DESCRIPTION file
usethis::use_package("cli", "Imports")
usethis::use_package("config", "Suggests")
usethis::use_package("DBI", "Imports")
usethis::use_package("dbplyr", "Imports")
usethis::use_package("dplyr", "Imports")
usethis::use_package("ggplot2", "Suggests")
usethis::use_package("haven", "Imports")
usethis::use_package("hms", "Imports")
usethis::use_package("janitor", "Suggests")
usethis::use_package("jsonlite", "Imports")
usethis::use_package("kableExtra", "Suggests")
usethis::use_package("knitr", "Imports")
usethis::use_package("lubridate", "Imports")
usethis::use_package("mirai", "Imports")
usethis::use_package("mori", "Imports")
usethis::use_package("parallelly", "Imports")
usethis::use_package("PhysicalActivity", "Suggests")
usethis::use_package("purrr", "Imports")
usethis::use_package("quarto", "Suggests")
usethis::use_package("R6", "Imports")
usethis::use_package("readr", "Imports")
usethis::use_package("rlang", "Imports")
usethis::use_package("RSQLite", "Imports")
usethis::use_package("scales", "Suggests")
usethis::use_package("stats", "Imports")
usethis::use_package("stringr", "Imports")
usethis::use_package("testthat", "Suggests")
usethis::use_package("tibble", "Suggests")
usethis::use_package("tidyr", "Imports")
usethis::use_package("utils", "Imports")
usethis::use_package("zoo", "Imports")

# Create citation file
usethis::use_citation()

# Create NAMESPACE file
usethis::use_namespace()

# Create license
usethis::use_mit_license(copyright_holder = "His Majesty the King in Right of Canada, as represented by Statistics Canada")

# Create tests folder
usethis::use_testthat()

# Create tests
usethis::use_test("001-get-chms-meta")
usethis::use_test("002-load-agd-settings")
usethis::use_test("003-agd-run")
usethis::use_test("004-render-sanity-check-report")
usethis::use_test("005-validate-barreira-algo")

# Run individual tests
testthat::test_file("tests/testthat/test-001-get-chms-meta.R")
testthat::test_file("tests/testthat/test-002-load-agd-settings.R")
testthat::test_file("tests/testthat/test-003-agd-run.R")
testthat::test_file("tests/testthat/test-004-render-sanity-check-report.R")
testthat::test_file("tests/testthat/test-005-validate-barreira-algo.R")

# Run all tests
devtools::test()

# Update NAMESPACE and create documentation
devtools::check_man()

# Run CRAN check
devtools::check(
  cran = TRUE,
  remote = TRUE,
  force_suggests = TRUE,
  args = "--as-cran",
  error_on = "error"
)

# Check with CRAN's incoming checks
urlchecker::url_check()

# Run all examples
devtools::run_examples(fresh = TRUE)

# Build package
package_build_path <- devtools::build()

# Run CRAN check
devtools::check_built(
  path = package_build_path,
  cran = TRUE,
  remote = TRUE,
  force_suggests = TRUE,
  args = "--as-cran",
  error_on = "error"
)
