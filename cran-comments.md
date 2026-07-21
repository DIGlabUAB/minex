## Resubmission

This is a resubmission. minex 0.1.0 is already published on CRAN (2026-07-16).
This submission is minex 0.2.0, a feature release: not a new package.

## R CMD check results

Local `R CMD check --as-cran` (macOS): 0 errors | 0 warnings | 0 notes.

## Test environments

* local macOS -- `R CMD check --as-cran`: OK

Multi-platform checks (win-builder R-devel/release and the GitHub Actions
matrix: Windows release, macOS release, Ubuntu devel/release/oldrel-1) are to
be run by the maintainer immediately before submission and are not yet
reflected here.

## Notes

* A second author (Sumanth Chandrupatla) was added to `Authors@R` in this
  release. The maintainer (`cre`) is unchanged from 0.1.0 (Sandeep
  Bodduluri).
* `clipr` is a new `Suggests` dependency, used only for the optional
  `minex(clipboard = TRUE)` input source; it is guarded by
  `requireNamespace()` and the example that exercises it is wrapped in
  `\dontrun{}`.
