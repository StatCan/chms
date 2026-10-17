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
