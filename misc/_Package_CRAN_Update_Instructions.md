# Instructions for updating the package in CRAN

* make changes to your package
  * try your best to follow the tidyverse style guide
* update DESCRIPTION with new version number
* update the NEWS
* R>> devtools::build()
  * green check marks are good, red x's are bad.  fix them
  * this step makes sure you can build the package
* R>> devtools::check()
  * updates package documentation
  * checks for issues
  * this is the first level of checks for CRAN issues.  It is the easiest CRAN check to read
* update cran-comments.md (including the # errors | # warnings | # notes)
* from within the RStudio terminal, run>> R CMD check --as-cran xgxr_#.#.#.tar.gz
  * (this tar.gz was built from the devtools::build() command)
  * doing this from within RStudio makes sure that the right version of R is used (we think)
* If there are any warnings or notes, open the document where they are saved and resolve them.
* Check the package into Github.  Wait a few minutes and check that the build is "passing" under Travis CI.
* Send the package to CRAN.  There are two options
  * devtools::release() - this will run a lot of checks that are helpful, but doesn't work with Novartis firewall.  It sometimes fails works on the GuestWireless network
  * Go to this website and upload the tar.gz https://CRAN.R-project.org/submit.html
* CRAN will send you an email saying the package has been submitted and you then need to click a link to confirm the submission.
