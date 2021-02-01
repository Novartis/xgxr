# Instructions for updating the package in CRAN

This doesn't quite work right.  Not sure how to get vignette to use the newest package.  Maybe need to add a devtools::load_all() to the top?

```
library("devtools")
load_all()
build_vignettes()
browseVignettes("xgxr")
```
