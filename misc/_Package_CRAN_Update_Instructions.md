# Instructions for updating the package in CRAN

* make changes to your package
  * try your best to follow the tidyverse style guide
* update cran-comments.md
* update DESCRIPTION with new version number
* update the NEWS
* run command>> devtools::build()
  * green check marks are good, red x's are bad
* run command>> devtools::check()
  * checks for issues
* from terminal, run>> R CMD check --as-cran xgxr_1.0.4.tar.gz
  * (this tar.gz was built from the devtools::build() command)
* If there are any warnings or notes, open the document where they are saved and resolve them.
* Check the package into Github.  Wait a few minutes and check that the build is "passing" under Travis CI.
* Send the package to CRAN.  There are two options
  * devtools::release() - this will run a lot of checks that are helpful, but doesn't work with Novartis firewall.  It sometimes works on the GuestWireless network
  * Go to this website and upload the tar.gz https://cran.r-project.org/submit.html
