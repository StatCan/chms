# chms 7.1.9000

## Bug fixes

* Fixed a bug on Linux affecting parallel processing in `agd$run()` with
  `cpu_max > 1`, where data shared with mirai workers via `mori::share()`
  caused jobs to fail.

## Internal changes

* Linted the package code with lintr to follow the tidyverse style guide.
  These changes do not affect behaviour.
