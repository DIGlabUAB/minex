# minex 0.3.0

* `minex(granularity = "expression")` reduces *within* a statement, isolating a
  failing pipeline stage (`|>`/`%>%`) or positional call argument via
  Hierarchical Delta Debugging (HDD*). The default `"statement"` is unchanged.
* The result gains `n_chars_original` and `n_chars_minimal` (character counts
  of the code before and after reduction) and a `granularity` field recording
  which mode produced the result.

Bug fixes:

* `ddmin()` (and `minex()`/`reduce_rows()`) now return the smallest *confirmed*
  subset when `max_oracle_calls` is exhausted mid-reduction, instead of the whole
  input. The result was already labelled `complete = FALSE`; it now
  also keeps the reduction confirmed before the budget ran out.
* `oracle_calls` now includes the one-off failure-point truncation probe, so the
  reported count reflects every predicate evaluation (the probe was previously
  uncounted).
* `granularity = "expression"` now drops any statement left redundant by
  sub-expression reduction (for example when an argument is stripped from a later
  call), keeping the result statement-minimal.
* Fixed a case under the in-process backend where a script that recovered from a
  warning promoted to an error via `options(warn = 2)` and then failed for a
  different reason could record the wrong failure.

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
