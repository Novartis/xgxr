## Test environments

* macOS 10.15.7, R 4.0.3
* Linux, x86_64-pc-linux-gnu, R 3.6.1
* Ubuntu 16.04.6 LTS (on Travis CI), R 3.6.1
* Fedora, R-devel, clang, gfortran (on R-hub)
* Ubuntu 16.04 LTS, R-release, GCC (on R-hub)
* Windows Server 2008 R2 SP1, R-devel (on R-hub)

## R CMD check results

0 errors | 0 warnings | 0 notes

**Comments:**

    o fix bug in xgx_stat_smooth / predictdf.nls that caused an error when formula had only one parameter to fit
    o add predict.nls function to produce predictions and confidence intervals for nls objects, similar to predict.lm for lm objects

## Downstream dependencies

None.
