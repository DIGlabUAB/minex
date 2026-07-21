# minex 0.2.0

* `minex()` can target warnings and messages, not just errors, via `condition`.
* Failure-point truncation drops statements after the failure for free.
* `max_oracle_calls` bounds the search; incomplete results are labelled
  `complete = FALSE`, print a note, and emit a warning.
* New `algorithm = "cdd"` (Counting-based Delta Debugging, Zhang et al. 2025)
  alongside the classic `"ddmin"`. **`"cdd"` is now the default algorithm**
  for `minex()`, `ddmin()`, and `reduce_rows()`; the classic `"ddmin"` block-
  halving loop remains available via `algorithm = "ddmin"`.
* `match` now also accepts a function for custom condition matching.
* `minex(clipboard = TRUE)` reads the script from the system clipboard
  (requires the suggested `clipr` package).

# minex 0.1.0

* Initial release.
* `minex()` reduces a failing R script to a one-minimal reproducible example,
  evaluating candidates in a separate R process by default.
* `ddmin()` exposes the underlying delta debugging algorithm for reuse on any
  collection.
* `reduce_rows()` reduces a data frame to the rows that reproduce a failure.
