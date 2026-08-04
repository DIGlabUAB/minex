# minex 0.2.0

* `explain_failure()` sends a `minex()` result to an LLM (via the suggested
  `ellmer` package), returning a structured explanation of why it fails, a
  proposed fix (verified by executing it), a paste-ready bug report, and a
  diagnosis. Provider-agnostic: works with any `ellmer`-supported backend
  (OpenAI, Anthropic, Ollama, OpenRouter, etc.).
* `minex(granularity = "expression")` reduces *within* a statement, isolating
  a failing pipeline stage (`|>`/`%>%`) or positional call argument via
  Hierarchical Delta Debugging (HDD*).
* **`minex()` now retries at `granularity = "expression"` when statement-level
  reduction removes nothing**, instead of returning the input unchanged. This
  is the usual outcome for a script that is one function definition plus a
  call: every top-level statement is load-bearing, but the failure is nested
  inside the function body where statement bisection cannot reach it. Measured
  on 61 such scripts, the share that reduce at all went from 10/61 to 61/61,
  removing a median 47% of characters. The retried result carries
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
* The result gains `n_chars_original` and `n_chars_minimal` (character counts
  of the code before and after reduction) and a `granularity` field recording
  which mode produced the result.
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
