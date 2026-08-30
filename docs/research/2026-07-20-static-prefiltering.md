# Static dependency pre-filtering: research notes

Date: 2026-07-20
Status: **investigated and rejected for v1**

## The idea

Before running delta debugging, build a def-use graph over top-level statements,
find which ones the failing statement transitively depends on, and discard the
provably-irrelevant remainder with **zero** oracle calls.

## Verdict: do not build for v1

There is no retained prototype, benchmark corpus, or result set that supports a
quantitative effectiveness claim. The decision is therefore based only on the
following conservative design analysis, not on measured yield.

## Why the expected scope is narrow

The proposed filter must conservatively retain:

- `library()` calls -- always kept, and ddmin's own block search removes them
  cheaply anyway
- linear pipelines where each step genuinely feeds the next -- nothing to prune
- calls to third-party plotting/modelling functions whose purity cannot be
  verified -- any assignment through them is stuck

Under the proposed conservative rules, the clearest removable cases are simple
arithmetic or base-function assignments to genuinely unused variables. No
preserved evidence currently quantifies how often those cases occur.

## Why not `CodeDepends`

`CodeDepends` provides related analysis via `getInputs()`/`readScript()` and a
tree-shaking helper, `getDependsThread()`. It was not adopted for two design
reasons.

**1. It imports the Bioconductor package `graph`.** For an MIT package whose only
current dependency is `callr`, adding a Bioconductor transitive dependency would
substantially increase the installation and maintenance surface.

**2. Dynamic evaluation and non-local assignment require explicit validation.**
Constructs such as `<<-`, `eval(parse())`, and `assign()` can invalidate a
static dependency approximation. Because no reproducible compatibility tests
are retained here, the behavior of `CodeDepends` on these cases is not asserted.

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

`ddmin()` unconditionally tests the full set first and stops if it does not
reproduce:

```r
if (!test(seq_len(n_items), exempt = TRUE)) {
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
