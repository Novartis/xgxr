## Test environments

* macOS 10.14.3, R 3.5.2
* Windows 10 1803, R 3.6.1
* Ubuntu 16.04.6 LTS (on Travis CI), R 3.6.1
* Fedora, R-devel, clang, gfortran (on R-hub)
* Ubuntu 16.04 LTS, R-release, GCC (on R-hub)
* Windows Server 2008 R2 SP1, R-devel (on R-hub)

## R CMD check results

0 errors | 0 warnings | 0 notes

**Comments:**

* reran usethis::use_data() on all datasets in R-3.6.1, because some versions of this command were run with R-3.5 and the datasets were not visible for an xgxr user.  
* we now require R-3.5 for using this package

## Downstream dependencies

None.
