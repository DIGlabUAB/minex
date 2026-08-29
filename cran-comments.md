## Update

This submission updates minex 0.1.0, published on CRAN on 2026-07-16, to
version 0.2.0. This is a feature release, not a new package.

## Change of maintainer

The maintainer (`cre`) changes in this release, from Sandeep Bodduluri
(`sbodduluri@uabmc.edu`, the 0.1.0 maintainer) to Sumanth Chandrupatla
(`srchandr@uab.edu`). Sandeep Bodduluri remains an author (`aut`) and
copyright holder (`cph`); no contributor has been removed.

The outgoing maintainer, Sandeep Bodduluri, has reviewed and approved this
change of maintainer.

## R CMD check results

Local `R CMD check --as-cran`: 0 errors | 0 warnings | 2 notes.

* CRAN incoming feasibility identifies Sumanth Chandrupatla as the new
  maintainer. This expected note is explained under "Change of maintainer"
  above.
* HTML manual validation was skipped because the locally installed HTML Tidy
  is not recent enough. The PDF and HTML manuals were generated successfully,
  and this local toolchain note did not occur in the GitHub Actions checks.

## Test environments

* Local: macOS (aarch64-apple-darwin20), R 4.5.2: OK (two notes explained
  above)
* GitHub Actions: macOS, R-release: OK
* GitHub Actions: Windows, R-release: OK
* GitHub Actions: Ubuntu, R-devel: OK
* GitHub Actions: Ubuntu, R-release: OK
* GitHub Actions: Ubuntu, R-oldrel-1: OK

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
