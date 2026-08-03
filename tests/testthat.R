# testthat is a Suggests dependency, so it is absent under check flavors that
# install Imports only (CRAN's noSuggests flavor, _R_CHECK_FORCE_SUGGESTS_=false).
# Guard the load so those runs skip the suite instead of erroring, and report the
# skip so a run that tested nothing is never mistaken for a run that passed.
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(minex)

  test_check("minex")
} else {
  cat("testthat not installed; minex test suite skipped\n")
}
