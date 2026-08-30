# Delta debugging algorithms: literature survey

Date: 2026-07-20
Status: research notes; implementation outcomes updated after 0.2.0
Scope: reducing **oracle call count** (not wall-clock constant factors)

## Why this matters for minex

By default, every oracle call spawns a fresh R process via `callr`, so oracle-call
count is an important design metric. This repository does not retain a timing
study that separates process startup from user-code execution.

The current engine (`R/ddmin.R`) defaults to a CDD-inspired reducer with
index-set memoization and a repeated singleton verification sweep. Classic
ddmin remains available with `algorithm = "ddmin"`. Neither strategy assumes
that the predicate is monotonic.

### Where the O(n^2) actually comes from

The two reduction paths have opposite cost profiles:

| Path | Implementation | Wins when | Cost |
|---|---|---|---|
| Reduce to *subset* (one block reproduces alone) | `ddmin_classic()` | result is **small** | halves input per hit, ~O(log n) |
| Reduce to *complement* (drop one block) | `ddmin_classic()` | result is **large** | shaves one block per hit, ~O(n^2) |

This complexity discussion is qualitative; no retained benchmark in this
repository establishes a representative call-count range for user scripts.

---

## Ranked candidates

| Rank | Algorithm | Citation | Relevance | Status |
|---|---|---|---|---|
| 1 | **CDD** (Counter-Based DD) | Zhang, Xu, Tian, Cheng & Sun, ICSE 2025 | Counter-based block sizing; the paper reports 52.03% fewer queries and 29.91% less time than ddmin across 76 benchmarks | **Adapted in 0.2.0** |
| 2 | ProbDD | Wang, Shen, Chen, Xiong & Zhang, ESEC/FSE 2021 | Probabilistic predecessor analyzed by the CDD paper | Not implemented |
| 3 | PMA | Tao & Xue, EASE 2025 | Probabilistic monotonicity assessment | Future research |
| 4 | Greedy static prefilter | No specific source | Potentially avoids oracle calls, but no retained effectiveness evidence | Rejected; see static-prefiltering notes |
| 5 | **DDMIN\*** | Vince & Kiss, JSEP 2024 | Repeatedly invokes DDMIN to a fixed point | Related motivation; not implemented |
| 6 | HDD/HDD* and successors | Misherghi & Su, ICSE 2006 + successors | Structure-aware reduction | A specialized HDD*-style pass was adopted in 0.2.0 |
| 7 | Perses | Sun, Li, Zhang, Gu & Su, ICSE 2018 | Grammar-guided program reduction | Future research |
| 8 | WDD | Zhou, Xu, Zhang, Tian & Sun, ICSE 2025 | Weighted delta debugging | Future research |
| 9 | Vulcan | Xu, Tian, Zhang, Zhao, Jiang & Sun, OOPSLA 2023 | Reduction beyond 1-minimality | Not aligned with current call-count goal |

---

## Decision for the near term: CDD + singleton sweep

### The key theoretical result

ICSE 2025 proves ProbDD's Bayesian "probabilities" are **monotonically
increasing round counters in disguise** (Lemma III.2: `p_r = p_0 / (1-e^-1)^r`).
The speedup comes from *skipping redundant queries*, not from probabilistic
inference. In the paper, CDD reproduces the benefit with a closed form and is
statistically indistinguishable from ProbDD for final size, execution time, and
query count (p = 0.42, 0.29, and 0.70, respectively).

The simplified block-size model motivated the dependency-light adaptation in
this package.

### The catch: CDD is not 1-minimal

Neither CDD nor ProbDD guarantees one-minimality. The paper reports that 1.11%
of 6,871 **ProbDD** invocations on one benchmark suite were not one-minimal,
with 1.49 additional removable elements on average among those invocations; it
does not report that exact failure rate for CDD.

**This conflicts with a documented guarantee.** `ddmin()` promises a
one-minimal result, and that promise is the package's core claim. Adopting CDD
without mitigation could silently make the documentation false.

Adaptation in `minex`: the reducer uses CDD's round-growth and block-size
equations, but recomputes the initial probability for the shortened candidate
and restarts the round counter after a successful deletion. A bounded
singleton-deletion sweep then restores one-minimality. This shares the
fixed-point goal studied by Vince & Kiss (2024), but it is not their DDMIN*
algorithm, which repeatedly invokes the complete DDMIN algorithm.

### Two-phase engine

```
Phase A  CDD-inspired rounds       closed-form block sizing, skips redundant queries
Phase B  singleton sweep           re-test singles to convergence -> restores 1-minimality
```

Implementation notes:

- The existing index-set cache in `ddmin()` is unchanged. CDD alters only
  *which* subsets are proposed, not the caching contract.
- `s_r` (the expected-gain-maximising block size) has a continuous relaxation
  `s_r ~ -1/ln(1-p_r)`, but `len` is small enough to just take the discrete
  argmax over `1..len` directly.
- Unlike Algorithm 2 in Zhang et al., Phase A resets its round counter and
  recomputes its initial probability after a successful deletion. The public
  documentation therefore describes this as a CDD adaptation.

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
10.1145/1134285.1134307). The original paper defines both HDD and its iterative
HDD* variant, which runs HDD to a fixed point and guarantees
*1-tree-minimality* (no single tree node removable while preserving the
property). Successors:

- Coarse HDD (Hodovan, Kiss & Gyimothy, ICSME 2017, DOI
  10.1109/ICSME.2017.26)
- HDDr, a recursive variant (Kiss, Hodovan & Gyimothy, A-TEST 2018, DOI
  10.1145/3278186.3278189)
- Picireny / "Modernizing HDD" (Hodovan & Kiss, A-TEST 2016, DOI
  10.1145/2994291.2994296)

**Perses -- syntax-guided program reduction** (Sun, Li, Zhang, Gu & Su, ICSE
2018, DOI 10.1145/3180155.3180236). Uses the grammar to generate syntactically
valid candidates. It is a much larger engineering lift because it needs a real
grammar for the target language.

### Why R is unusually well suited to this

R exposes its own AST natively: `parse()` returns a real expression tree,
`keep.source = TRUE` preserves original formatting via `srcref`, and
`as.list()`/`[[` walk call objects directly. No external grammar or parser
generator is required for the supported reductions.

### Implemented approach

The package uses a specialized **HDD*-style** pass over R's parse data. It calls
`ddmin()` at each supported tree depth and repeats passes to a fixed point, but
only defines deletion spans for pipeline stages and positional call arguments;
it is not a general implementation over every AST node. Perses remains a
possible future refinement.

---

## Citation hygiene -- MUST verify before shipping

`minex` cites sources in Rd docs and the vignette, so every DOI must be checked
against a publisher or archival record before it appears in shipped
documentation. Comparative performance figures not needed to explain the
implemented methods have been removed. The retained CDD figures and the
ProbDD-specific minimality result were verified against the ICSE 2025 paper.

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
  *A-TEST 2016*. DOI 10.1145/2994291.2994296.
- Hodovan, R., Kiss, A. & Gyimothy, T. (2017). "Coarse Hierarchical Delta
  Debugging." *ICSME 2017*, 194-203. DOI 10.1109/ICSME.2017.26.
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
  DOI 10.1145/2345156.2254104.
