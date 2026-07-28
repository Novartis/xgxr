## Submission

This is a bug-fix release. `xgx_stat_ci()` ignored the position scale when the
user supplied the `breaks` argument, so combining `breaks` with a transformed
axis such as `xgx_scale_x_log10()` silently binned the data incorrectly. See
NEWS for the full list of changes.

The previous CRAN release was 1.1.2; versions 1.1.3 through 1.1.5 were
developed but never submitted, so this submission also carries those changes.

## Test environments

* local: macOS 14.4.1 (aarch64-apple-darwin20), R 4.5.3, ggplot2 4.0.2
* GitHub Actions: macOS-latest (R release), Windows-latest (R release),
  Ubuntu-latest (R devel, release, oldrel-1)
* TODO before submitting: devtools::check_win_devel()
* TODO before submitting: R-hub

## R CMD check results

Local `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes

**Comments:**

## Downstream dependencies

None.
