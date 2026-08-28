# Static dependency pre-filtering: research notes

Date: 2026-07-20
Status: **investigated and rejected for v1**
Tested on: R 4.5.2, `codetools` (base), `CodeDepends` 0.6.7

## The idea

Before running delta debugging, build a def-use graph over top-level statements,
find which ones the failing statement transitively depends on, and discard the
provably-irrelevant remainder with **zero** oracle calls.

## Verdict: do not build for v1

Measured yield is too low to justify the code and maintenance surface.

## Why: the measured number

A prototype implementing the full conservative algorithm (below) was run against
a 16-statement synthetic analysis script **deliberately constructed to favour
the filter** -- containing two `library()` calls, `read.csv`, a real dplyr
pipeline, several genuinely dead statements, and a failing `stopifnot()`.

**Result: 2 of 16 statements dropped (12.5%).**

Most of the planted dead statements were *not* droppable:

| Statement | Dropped? | Why not |
|---|---|---|
| `summary_stats <- summary(raw)` | no | `summary` not in the purity whitelist |
| `unused_plot <- ggplot(...)` | no | `ggplot` not in the purity whitelist |
| `noise <- rnorm(...)` | no | RNG is impure -- correctly kept, dropping would shift downstream random state |
| `cleaned$flag <- ...` | no | complex LHS, kept by construction |
| `set.seed(1)` | no | bare call, kept by construction |

This generalises. Realistic R analysis scripts are dominated by:

- `library()` calls -- always kept, and ddmin's own block search removes them
  cheaply anyway
- linear pipelines where each step genuinely feeds the next -- nothing to prune
- calls to third-party plotting/modelling functions whose purity cannot be
  verified -- any assignment through them is stuck

The only statements a *sound* filter reliably removes are simple
arithmetic/base-function assignments to genuinely unused variables. Real, but
narrow. Expect **10-25%** on messy exploratory scripts and **~0%** on tight
pipelines.

## Why not `CodeDepends`

`CodeDepends` (CRAN, GPL-2|GPL-3, v0.6.7, actively maintained) does exactly this
analysis via `getInputs()`/`readScript()` and even ships a tree-shaking helper,
`getDependsThread()`. Disqualified on two independent grounds.

**1. It hard-`Imports` the Bioconductor package `graph`,** which is not on CRAN.
Installing it required `BiocManager::install("graph")`. For an MIT package whose
only current dependency is `callr`, adding a Bioconductor-only transitive
dependency is a large, fragile cost -- CRAN/Bioconductor release-cycle mismatch
is a common source of `R CMD check` breakage.

**2. Its own tree-shaking function is empirically unsound** on all three hazards
tested. It silently under-approximates rather than erroring:

| Case | `getDependsThread()` returned | Should have included |
|---|---|---|
| `x <<- 99` in a function, later `print(x)` | `{4}` | the defining statements |
| `eval(parse(text = "z <- 10"))`, later `w <- z + 1` | `{3}` | statement 1 |
| `assign(nm, 5)` with `nm` a variable | `{4}` | statements 1-2 |

In the `eval(parse())` case the signal *exists* in the object (`getInputs()`
correctly records `functions: eval, parse`) but the pruning function ignores it.
The `assign()` case emits a `warning()` to stderr, but that warning is **not**
reflected in the returned `ScriptNodeInfo@sideEffects` field, so a caller
inspecting structured output alone would never see it.

A mature, purpose-built package gets this wrong on exactly the hazards that
matter. That is strong evidence the problem is harder than it looks.

## Tooling notes (if ever revisited)

- `codetools::findGlobals()` works only on closures, not raw top-level
  expressions -- each statement must be wrapped via
  `eval(call("function", NULL, expr))` first. It does not separate reads from
  writes and reports operators as pseudo-globals.
- `all.vars()` / `all.names()` on a parsed expression are the more practical
  primitive, but also do not separate LHS from RHS. That requires a hand-rolled
  recursive walker matching on `<-`, `=`, `<<-` call heads, descending into
  `if`/`for`/`while`/`{}` but **not** into nested `function(){}` bodies except
  through `<<-`.

Verified walker behaviour:

| Expression | Outer writes |
|---|---|
| `if (x > 0) y <- 1 else y <- 2` | `{y}` |
| `for (i in 1:10) s <- s + i` | `{s}` |
| `g <- function(a) { a <- a + 1; a }` | `{g}` (local `a` correctly excluded) |
| `g <- function(a) { z <<- a + 1 }` | `{g, z}` |

## The algorithm, if revisited

Two-part conservative rule:

1. **Global bail-out gate.** If *any* statement calls a blacklisted opacity
   function -- `eval, parse, get, get0, mget, dynGet, do.call, assign,
   delayedAssign, makeActiveBinding, sys.call, sys.function, match.call,
   environment, as.environment, list2env, attach, sys.source, source, local,
   with, within, substitute, bquote` -- disable the filter entirely for that
   script. Once a statement can read or write an unbounded set of names, no
   read-set anywhere in the script can be trusted.

2. **Backward reachability + purity whitelist.** Compute the target statement's
   reads; fixed-point backward scan keeping any statement whose writes intersect
   the needed set. A statement is removable only if it is a *simple* assignment
   (bare symbol LHS -- not `x[i] <-`, `x$a <-`, `names(x) <-`) to an unneeded
   symbol, **and** every function in its RHS is on a hand-curated purity
   whitelist.

Safe over-approximations (reduce yield, never cause unsound drops): NSE like
`subset()` and formulas cause `all.vars` to treat column names as globals.
`substitute`/`bquote` construct code dynamically and must be blacklisted.

## Why an unsound filter is mostly self-defeating

Worth recording, because it lowers the risk if this is ever revisited.

`ddmin()` unconditionally tests the full set first (`R/ddmin.R:68-71`) and hard
`stop()`s if it does not reproduce:

```r
if (!test(seq_len(n_items))) {
  stop("`interesting` is FALSE for the full set; nothing to minimize.", call. = FALSE)
}
```

- Under the default `match = "message"`, an unsound drop almost always makes the
  script fail *differently* (`object 'x' not found` rather than the target
  message), the precondition fails immediately, and `minex()` aborts loudly.
  The failure mode is a crash, not silent corruption.
- Under `match = "class"` / `"both"`, a coincidentally-similar generic
  `simpleError` could pass. But this weakness is **inherent to class matching
  itself** -- any subset ddmin explores mid-search could already hit it. An
  unsound filter does not create a new failure mode, it just makes the
  pre-existing one slightly more likely to appear at the starting candidate.

## If revisited

Ship as an experimental opt-in `static_prefilter = TRUE`, never default-on,
hand-rolled with no new dependency, gated by the bail-out rule, with a dedicated
test suite proving it never breaks ddmin's full-set precondition across a
curated corpus of real scripts.

Trigger for revisiting: evidence that oracle-call count on large (100+
statement) messy scripts is a real user pain point that truncation and CDD did
not already solve.
