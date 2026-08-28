## Resubmission

This is a resubmission. minex 0.1.0 is already published on CRAN (2026-07-16).
This submission is minex 0.2.0, a feature release: not a new package.

## Change of maintainer

The maintainer (`cre`) changes in this release, from Sandeep Bodduluri
(`sbodduluri@uabmc.edu`, the 0.1.0 maintainer) to Sumanth Chandrupatla
(`srchandr@uab.edu`). Sandeep Bodduluri remains an author (`aut`) and
copyright holder (`cph`); no contributor has been removed.

The outgoing maintainer, Sandeep Bodduluri, has reviewed and approved this
change of maintainer.

## R CMD check results

Local `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes.

## Test environments

* local -- macOS 15 (aarch64-apple-darwin20), R 4.5.2: OK

Multi-platform checks (win-builder R-devel/release and the GitHub Actions
matrix: Windows release, macOS release, Ubuntu devel/release/oldrel-1) are to
be run by the maintainer immediately before submission and are not yet
reflected here.

## Notes

* `clipr` is a new `Suggests` dependency, used only for the optional
  `minex(clipboard = TRUE)` input source; it is guarded by
  `requireNamespace()` and the example that exercises it is wrapped in
  `\dontrun{}`.
* `ellmer` is a new `Suggests` dependency, used only by the optional
  `explain_failure()` helper, which sends a reduced example to a
  user-configured LLM. It is guarded by `requireNamespace()`, requires the
  user to supply their own chat object, and performs no network access at
  check time: every example and test that touches it is either skipped when
  no chat is configured or wrapped in `\dontrun{}`.
