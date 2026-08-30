# minex (development version)

* Corrected the expansion of CDD to Counter-Based Delta Debugging and added
  primary citations for CDD and HDD/HDD*, plus related work on fixed-point
  minimization, throughout the package documentation. Documentation now
  distinguishes the package's adaptations from the papers' exact algorithms.
* Clarified that LLM-proposed fixes run in an isolated process, not an operating
  system sandbox, and that prompt framing and the call denylist are mitigations,
  not security boundaries.

# minex 0.2.0

* `explain_failure()` sends a `minex()` result to an LLM (via the suggested
  `ellmer` package), returning a structured explanation of why it fails, a
  proposed fix (verified by executing it), a paste-ready bug report, and a
  diagnosis. Provider-agnostic: works with any `ellmer`-supported backend
  (OpenAI, Anthropic, Ollama, OpenRouter, etc.).
* `minex(granularity = "expression")` reduces *within* a statement, isolating
  a failing pipeline stage (`|>`/`%>%`) or positional call argument via a
  specialized HDD*-style pass (Misherghi and Su, 2006,
  <doi:10.1145/1134285.1134307>).
* **`minex()` now retries at `granularity = "expression"` when statement-level
  reduction removes nothing**, instead of returning the input unchanged. This
  is the usual outcome for a script that is one function definition plus a
  call: every top-level statement is load-bearing, but the failure is nested
  inside the function body where statement bisection cannot reach it. The
  retried result carries
  `escalated_from = "statement"` and `coarse_oracle_calls`, and its `code` is a
  simplification of the original statements rather than a subset of them. Pass
  `granularity = "statement"` explicitly for statement-level reduction only.
  `max_oracle_calls` still bounds the whole call -- the second pass runs on
  what the first left -- and no retry happens when the statement pass stopped
  early against that budget.
* Input that does not parse now raises a classed condition,
  `c("minex_parse_error", "minex_error")`, preserving R's parser diagnostic in
  both the message and a `parse_error` field. Callers that feed `minex()`
  machine-generated code can branch on the class rather than matching against
  the text of a parser message.
* `print()` on a result leads with whichever axis actually moved. A run that
  removed characters but no whole statements previously headlined
  "3 statement(s) reduced to 3", reporting a successful reduction as a
  failure. When nothing moved at statement granularity, the note now names
  `granularity = "expression"`.
* The result gains `n_chars_original` and `n_chars_minimal` (character counts
  of the code before and after reduction) and a `granularity` field recording
  which mode produced the result.
* `minex()` can target warnings and messages, not just errors, via `condition`.
* Failure-point truncation drops statements after the failure for free.
* `max_oracle_calls` bounds the search; incomplete results are labelled
  `complete = FALSE`, print a note, and emit a warning.
* New `algorithm = "cdd"` (adapted from Counter-Based Delta Debugging,
  Zhang et al. 2025,
  <doi:10.1109/ICSE55347.2025.00117>)
  alongside the classic `"ddmin"`. **`"cdd"` is now the default algorithm**
  for `minex()`, `ddmin()`, and `reduce_rows()`; the classic `"ddmin"` block-
  halving loop remains available via `algorithm = "ddmin"`.
* `match` now also accepts a function for custom condition matching.
* `minex(clipboard = TRUE)` reads the script from the system clipboard
  (requires the suggested `clipr` package).

Bug fixes:

* `ddmin()` (and `minex()`/`reduce_rows()`) now return the smallest *confirmed*
  subset when `max_oracle_calls` is exhausted mid-reduction, instead of the whole
  input.
* `oracle_calls` now includes the one-off failure-point truncation probe, so the
  reported count reflects every predicate evaluation.
* `granularity = "expression"` now drops any statement left redundant by
  sub-expression reduction, keeping the result statement-minimal.
* Fixed a case under the in-process backend where a script that recovered from a
  warning promoted to an error via `options(warn = 2)` and then failed for a
  different reason could record the wrong failure.

# minex 0.1.0

* Initial release.
* `minex()` reduces a failing R script to a one-minimal reproducible example,
  evaluating candidates in a separate R process by default.
* `ddmin()` exposes the underlying delta debugging algorithm for reuse on any
  collection.
* `reduce_rows()` reduces a data frame to the rows that reproduce a failure.
