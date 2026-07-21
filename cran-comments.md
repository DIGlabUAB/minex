## Resubmission

This is a resubmission. minex 0.1.0 is already published on CRAN (2026-07-16).
This submission is minex 0.2.0, a feature release: not a new package.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS, R 4.4.2 -- `R CMD check --as-cran`: OK
* win-builder, R-devel -- OK
* GitHub Actions -- `R CMD check` passing on:
  * Windows (release)
  * macOS (release)
  * Ubuntu (devel, release, oldrel-1)

## Notes

* A second author (Sumanth Chandrupatla) was added to `Authors@R` in this
  release. The maintainer (`cre`) is unchanged from 0.1.0 (Sandeep
  Bodduluri).
* `clipr` is a new `Suggests` dependency, used only for the optional
  `minex(clipboard = TRUE)` input source; it is guarded by
  `requireNamespace()` and the example that exercises it is wrapped in
  `\dontrun{}`.
