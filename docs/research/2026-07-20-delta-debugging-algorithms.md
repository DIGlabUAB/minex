# Delta debugging algorithms: literature survey

Date: 2026-07-20
Status: research notes; implementation outcomes updated after 0.2.0
Scope: reducing **oracle call count** (not wall-clock constant factors)

## Why this matters for minex

Every oracle call spawns a fresh R process via `callr` (~300 ms), almost all of
which is interpreter startup rather than user code. So the dominant cost is the
*number* of calls, not the speed of each one. Wall-clock work (persistent
sessions, parallel evaluation) is deliberately deferred until the algorithm is
settled.

The current engine (`R/ddmin.R`) is classic ddmin with index-set memoization and
no monotonicity assumption. Worst case is O(n^2) oracle calls.

### Where the O(n^2) actually comes from

The two reduction paths have opposite cost profiles:

| Path | Code | Wins when | Cost |
|---|---|---|---|
| Reduce to *subset* (one block reproduces alone) | `R/ddmin.R:84` | result is **small** | halves input per hit, ~O(log n) |
| Reduce to *complement* (drop one block) | `R/ddmin.R:96` | result is **large** | shaves one block per hit, ~O(n^2) |

The worst case only materialises when the input barely reduces -- i.e. when the
tool had little to offer anyway. The realistic painful case is a middling one
(200 statements down to ~20), costing roughly 2,000-4,000 calls.

---

## Ranked candidates

| Rank | Algorithm | Citation | Improvement vs ddmin | 1-minimal? | LOC | Verdict |
|---|---|---|---|---|---|---|
| 1 | **CDD** (Counter-Based DD) | Zhang, Xu, Tian, Cheng & Sun, ICSE 2025 | ~52% fewer queries, ~27% less time (76 benchmarks) | **No** (~1.1% miss rate) | ~40-60 | **Adopted in 0.2.0** |
| 2 | ProbDD | Wang, Shen, Chen, Xiong & Zhang, ESEC/FSE 2021 | same order as CDD | No, in practice | ~150-250 | Dominated by CDD |
| 3 | PMA | Tao & Xue, EASE 2025 | +22% on ProbDD | claimed, unreplicated | high | Re-check in ~1 year |
| 4 | **Greedy O(n) pre-filter** | folklore (cf. C-Reduce passes) | additive, unquantified | **Yes, trivially** | ~15 | **Adopt** |
| 5 | **DD\* / fixed-point iteration** | Vince & Kiss, JSEP 2024 | restores minimality after a lossy pass | **Yes, by construction** | trivial | **Adopted as safety net** |
| 6 | HDD / Coarse HDD / HDDr | Misherghi & Su, ICSE 2006 + successors | 25-40% smaller output; HDDr 29-65% less time | Yes (1-tree-minimal) | high | **HDD* subset adopted in 0.2.0** |
| 7 | Perses | Sun, Li, Zhang, Gu & Su, ICSE 2018 | output 2% the size of ddmin's; time 23% of ddmin's | Yes, stronger | very high | **0.3.0 -- see below** |
| 8 | WDD | Zhou, Xu, Zhang, Tian & Sun, ICSE 2025 | numbers UNVERIFIED | inherits base | low-med | Orthogonal; revisit |
| 9 | Vulcan | Xu, Tian, Zhang, Zhao, Jiang & Sun, OOPSLA 2023 | smaller than 1-minimal, but **more** calls | beyond 1-minimal | high | **Wrong direction** for us |

---

## Decision for the near term: CDD + greedy pre-filter + fixpoint sweep

### The key theoretical result

ICSE 2025 proves ProbDD's Bayesian "probabilities" are **monotonically
increasing round counters in disguise** (Lemma III.2: `p_r = p_0 / (1-e^-1)^r`).
The speedup comes from *skipping redundant queries*, not from probabilistic
inference. CDD reproduces the benefit with a closed form -- statistically
indistinguishable from ProbDD (p = 0.10-0.87) at a third of the code.

This is a result that makes the implementation *smaller*, which suits a
dependency-light package.

### The catch: CDD is not 1-minimal

Empirically ~1.1% of runs miss one-minimality, averaging 1.49 extra removable
elements. ProbDD's proof holds only under an idealised complete-exploration
model; the practical query-skipping heuristic breaks it.

**This conflicts with a documented guarantee.** `R/ddmin.R:5-7` promises a
one-minimal result, and that promise is the package's core claim. Adopting CDD
without mitigation would make the documentation false ~1% of the time --
silently, in exactly the way that produces a bad bug report.

Mitigation: a bounded fixed-point sweep (Vince & Kiss 2024) after CDD converges. It only
does a second full pass when something is actually removable, so the amortised
cost is small relative to the savings.

### Three-phase engine

```
Phase A  greedy O(n) pre-filter    each removal oracle-verified -> safe
Phase B  CDD rounds                closed-form block sizing, skips redundant queries
Phase C  fixpoint sweep            re-test singles to convergence -> restores 1-minimality
```

Phases A and C are the same single-element-removal helper invoked at different
points, so it is one function used twice.

Implementation notes:

- The existing index-set cache (`R/ddmin.R:55-66`) is unchanged. CDD alters only
  *which* subsets are proposed, not the caching contract.
- `s_r` (the expected-gain-maximising block size) has a continuous relaxation
  `s_r ~ -1/ln(1-p_r)`, but `len` is small enough to just take the discrete
  argmax over `1..len` directly.
- Phase B resets its round counter on success, mirroring ddmin's `n <- 2L` reset.

---

## Rejected: the monotonicity fast-path

An `assume_monotonic = TRUE` flag was considered and **rejected**. If
monotonicity held, one O(n) linear pass would suffice. It does not hold for R
scripts, for domain-specific reasons:

- **Ordering dependencies.** Removing an earlier statement turns the target
  error into an unrelated "object not found" -- the predicate flips for the
  *wrong reason*, non-monotonically in both directions.
- **Side effects.** `library()`, `options()`, `set.seed()`, working directory,
  env vars -- removal moves behaviour either way, not just away from failure.
- **Non-determinism.** Without a fixed seed, "reproduces" is not a deterministic
  function of the subset at all, so monotonicity is not merely false but
  ill-defined.
- **Masking bugs.** A second bug that manifests only with more code present
  (timing, memory, `options(warn = 2)`) makes superset-reproduces false.
- **Lazy evaluation.** Removing an unrelated statement can change whether a
  promise is ever forced.

Corroboration: the PMA paper (EASE 2025) exists precisely because the assumption
fails in practice, and its remedy is a dynamic per-run confidence estimate, not
a user-supplied boolean.

Phase A's greedy pre-filter captures most cheap "monotonic-looking" wins
**without trusting anything**, since every removal is individually verified.

If ever revisited, the defensible form is a narrowly scoped
`find_earliest_failing_prefix()` sibling (a bisection over prefixes, closer to
`git bisect`), never a trust-me flag on `ddmin()`.

---

## Implemented in 0.2.0: sub-expression reduction (HDD*)

This section records the rationale that led to the expression-level HDD* pass
shipped in 0.2.0. The implementation deliberately supports the R constructs
with safe deletion semantics: pipeline stages and positional call arguments.

### The problem

`split_statements()` (`R/parse.R`) splits at **top-level expressions only**. So:

```r
library(dplyr)
result <- mtcars |>
  filter(cyl == 4) |>
  mutate(ratio = hp / wt) |>
  summarise(m = mean(nonexistent_col))
```

is 2 statements. `library(dplyr)` is required, so minex reduces 2 -> 2 and
delivers nothing, even though the culprit is one call inside the pipeline.

### Why plain ddmin cannot be pointed at sub-expressions

`ddmin()` assumes elements are **independently removable**. Sub-expressions are
not: dropping `filter()` from a pipeline is fine, but dropping `mtcars` leaves
`|> summarise(...)`, a **syntax error** rather than a smaller failing program.

### The two candidate approaches

**HDD -- Hierarchical Delta Debugging** (Misherghi & Su, ICSE 2006, DOI
10.1145/1134285.1134307). Runs ddmin level-by-level over the AST, so every
candidate is syntactically valid by construction. Guarantees *1-tree-minimality*
(no single tree node removable while preserving the property) -- strictly
stronger than flat 1-minimality. Successors:

- Coarse HDD (Hodovan, Kiss & Gyimothy, ICSME 2017)
- HDDr, a recursive variant (Kiss, Hodovan & Gyimothy, A-TEST 2018, DOI
  10.1145/3278186.3278189) -- 29-65% less time than baseline HDD
- Picireny / "Modernizing HDD" (Hodovan & Kiss, A-TEST 2016) -- 25-40% smaller
  output than flat HDD. *DOI unconfirmed.*

**Perses -- syntax-guided program reduction** (Sun, Li, Zhang, Gu & Su, ICSE
2018, DOI 10.1145/3180155.3180236). Uses the grammar to only ever generate
syntactically valid candidates. Reported output **2% the size of ddmin's** and
45% the size of HDD's; time 23%/47% of ddmin/HDD. Much larger engineering lift
-- needs a real grammar for the target language.

### Why R is unusually well suited to this

R exposes its own AST natively: `parse()` returns a real expression tree,
`keep.source = TRUE` preserves original formatting via `srcref`, and
`as.list()`/`[[` walk call objects directly. No external grammar or parser
generator is required -- which is the single biggest cost in HDD/Perses
implementations for other languages.

### Implemented approach

The package starts with **HDD** over R's parse data (the parser is free, the
guarantee is stronger, and the scope is bounded). It reuses the Phase A/B/C
engine at each reducible tree level and repeats passes to a fixed point (HDD*).
Perses remains a possible future refinement.

---

## Citation hygiene -- MUST verify before shipping

`minex` cites sources in Rd docs and the vignette, so every DOI must be checked
against the publisher page before it appears in shipped documentation. The
following were flagged as **not independently verified**:

- **WDD** quantitative claims -- only the abstract was recoverable. Do not quote
  percentages.
- **Picireny / "Modernizing HDD"** (A-TEST 2016, ACM DL id 2994296) -- exact DOI
  unconfirmed.
- **C-Reduce** (Regehr et al., PLDI 2012) -- two conflicting DOI strings seen
  (`10.1145/2345156.2254104` vs `10.1145/2254064.2254104`). dblp confirms venue
  and pages; the DOI needs a final check.
- **ProbDD** and **PMA** full texts could not be machine-extracted; their details
  are corroborated via the ICSE 2025 re-analysis rather than read directly.

Verified directly: the ICSE 2025 CDD paper (arXiv HTML), which is the source of
all query-count and 1-minimality figures quoted above.

---

## Full citation list

- Zeller, A. & Hildebrandt, R. (2002). "Simplifying and Isolating
  Failure-Inducing Input." *IEEE TSE* 28(2), 183-200. DOI 10.1109/32.988498.
  (Already cited in `R/ddmin.R`.)
- Zhang, M., Xu, Z., Tian, Y., Cheng, X. & Sun, C. (2025). "Toward a Better
  Understanding of Probabilistic Delta Debugging." *ICSE 2025*.
  DOI 10.1109/ICSE55347.2025.00117; arXiv:2408.04735. **Verified directly.**
- Wang, G., Shen, R., Chen, J., Xiong, Y. & Zhang, L. (2021). "Probabilistic
  Delta Debugging." *ESEC/FSE 2021*. DOI 10.1145/3468264.3468625.
- Tao, Y. & Xue, J. (2025). "Accelerating Delta Debugging through Probabilistic
  Monotonicity Assessment." *EASE 2025*. arXiv:2506.11614.
- Vince, D. & Kiss, A. (2024). "Evaluation of the Fixed-Point Iteration of Minimizing Delta
  Debugging." *J. Softw. Evol. Proc.* DOI 10.1002/smr.2702.
- Misherghi, G. & Su, Z. (2006). "HDD: Hierarchical Delta Debugging."
  *ICSE 2006*, 142-151. DOI 10.1145/1134285.1134307.
- Hodovan, R. & Kiss, A. (2016). "Modernizing Hierarchical Delta Debugging."
  *A-TEST 2016*. ACM DL id 2994296. *DOI unconfirmed.*
- Hodovan, R., Kiss, A. & Gyimothy, T. (2017). "Coarse Hierarchical Delta
  Debugging." *ICSME 2017*, 194-203.
- Kiss, A., Hodovan, R. & Gyimothy, T. (2018). "HDDr: A Recursive Variant of the
  Hierarchical Delta Debugging Algorithm." *A-TEST 2018*, 16-22.
  DOI 10.1145/3278186.3278189.
- Sun, C., Li, Y., Zhang, Q., Gu, T. & Su, Z. (2018). "Perses: Syntax-Guided
  Program Reduction." *ICSE 2018*, 361-371. DOI 10.1145/3180155.3180236.
- Xu, Z., Tian, Y., Zhang, M., Zhao, G., Jiang, Y. & Sun, C. (2023). "Pushing the
  Limit of 1-Minimality of Language-Agnostic Program Reduction" (Vulcan).
  *PACMPL* 7(OOPSLA1), 636-664. DOI 10.1145/3586049.
- Zhou, X., Xu, Z., Zhang, M., Tian, Y. & Sun, C. (2025). "WDD: Weighted Delta
  Debugging." *ICSE 2025*. DOI 10.1109/ICSE55347.2025.00071; arXiv:2411.19410.
  *Quantitative results unverified.*
- Regehr, J., Chen, Y., Cuoq, P., Eide, E., Ellison, C. & Yang, X. (2012).
  "Test-Case Reduction for C Compiler Bugs." *PLDI 2012*, 335-346.
  *DOI needs verification.*
