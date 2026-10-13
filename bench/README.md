# minex benchmark corpus

Git-tracked, `.Rbuildignore`'d (never ships to CRAN — it spawns many `callr`
processes and would blow the check-time budget).

Run from the package root:

```sh
Rscript bench/run-benchmark.R
```

Install the development dependency `pkgload` first. The harness loads the
package source and runs each script in `scripts/` under both
`algorithm = "cdd"` and `"ddmin"`, at statement and expression granularity,
using the isolated `callr` backend. This produces 24 cases for the current corpus.

Every case is printed and saved to a timestamped CSV in the ignored
`bench/results/` directory, including cases that fail. To choose a destination:

```sh
Rscript bench/run-benchmark.R --output /tmp/minex-benchmark.csv
```

The CSV records the script, algorithm, granularity, condition, backend, status
(`success`, `incomplete`, or `error`), elapsed seconds, completion flag, oracle
calls, original and reduced statement and character counts, unobserved candidate
evaluation count, error message and classes, and warning messages. Failed cases
have empty result metrics rather than disappearing from the table. Each row also
records the source package version, Git revision and whether the checkout has
uncommitted changes, R version, platform, and UTC run timestamp. Git fields are
empty if Git or checkout metadata is unavailable.
The command saves the complete table before exiting nonzero if any case fails or
returns an incomplete reduction.

Use `--backend inprocess` for a faster local diagnostic run. Record and compare
runs with the same backend, corpus, R version, and machine; elapsed times include
script loading and are affected by process startup and local system load.

Purpose: compare `"cdd"` (the current default) with `"ddmin"` and guide future
default decisions using oracle-call count and output size across the corpus. The
harness preserves results so failures and incomplete reductions remain visible.
This directory is a small regression/decision corpus, not evidence for a
package-wide performance claim. Inspect completion and failure status before
comparing the metrics.

The CI workflow runs the benchmark helper tests directly from the checkout,
because `bench/` is excluded from the package built for `R CMD check`.

## Scripts

- `01-trailing-setup.R` — culprit last, lots of irrelevant setup.
- `02-early-failure.R` — fails early with a long tail (truncation win).
- `03-tangled.R` — interdependent statements that must be kept together.
- `04-warning-target.R` — warns rather than errors (`condition = "warning"`).
- `05-pipeline.R` — expression-level reduction of a pipeline.
- `06-nested-call.R` — expression-level reduction of a nested call.
