# minex benchmark corpus

Git-tracked, `.Rbuildignore`'d (never ships to CRAN — it spawns many `callr`
processes and would blow the check-time budget).

Run from the package root:

```sh
Rscript bench/run-benchmark.R
```

It runs each script in `scripts/` under both `algorithm = "cdd"` and
`"ddmin"` and tabulates oracle-call count and result size (`n_minimal`).

Purpose: decide whether `algorithm = "cdd"` should be the default. Only prefer
`"cdd"` if it is at least as good as `"ddmin"` on **both** oracle-call count and
output size across the corpus; otherwise the default stays `"ddmin"`. The
harness prints results but does not preserve them, so this directory is a small
regression/decision corpus, not evidence for a package-wide performance claim.

## Scripts

- `01-trailing-setup.R` — culprit last, lots of irrelevant setup.
- `02-early-failure.R` — fails early with a long tail (truncation win).
- `03-tangled.R` — interdependent statements that must be kept together.
- `04-warning-target.R` — warns rather than errors (`condition = "warning"`).
- `05-pipeline.R` — expression-level reduction of a pipeline.
- `06-nested-call.R` — expression-level reduction of a nested call.
