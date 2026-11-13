# chms 7.3

## Bug fixes

* `amgdsda` now uses the correct sleep duration guideline for older adults.
  Respondents aged 65 and older now meet the guideline with 7 to less than
  9 hours of sleep, not 7 to less than 10 hours. The 7 to less than 10 hour
  range now applies only to adults aged 18 to 64.
  
## Internal changes

* Added `tools/process-cycle7-data.R`, a script that processes CHMS Cycle 7
  accelerometer data site by site (sites 2 to 18) with `agd$run()`. For each
  site, it saves `summary_full`, `summary_full_stc` and `summary_run` as CSV
  files in that site's `R processing` folder. File locations are read from
  the `cycle7_clinic_file` and `cycle7_agd_dir` settings in `config.yml`.

# chms 7.2.0

## Bug fixes

* Fixed a bug on Linux affecting parallel processing in `agd$run()` with
  `cpu_max > 1`, where data shared with mirai workers via `mori::share()`
  caused jobs to fail.
  
* Variables in `agd$results$summary_full_stc` are now reliably returned in
  the correct order.

* Dates are now more robustly sanitized and removed from sleep columns
  where timestamps are expected.
  
## Internal changes

* Linted the package code with lintr to follow the tidyverse style guide.
  These changes do not affect behaviour.
