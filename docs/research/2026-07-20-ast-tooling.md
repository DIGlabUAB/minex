# AST tooling for HDD source reconstruction: research notes

Date: 2026-07-20
Status: informs the 0.3.0 sub-expression-reduction milestone
Tested on: R 4.5.2 (verified via Rscript, not inferred)

## Question

Is there a CRAN-safe replacement for `CodeDepends`, for two use cases?

- **A (near-term):** def-use dependency analysis over top-level statements.
- **B (0.3.0):** AST traversal for Hierarchical Delta Debugging -- deleting
  subtrees and regenerating **valid, human-readable** R source. Hard part is
  source reconstruction, since a mutated tree has no `srcref`.

## Verdicts

**Use case A: verdict unchanged -- do not build / hand-roll if ever revisited.**
No tool investigated computes a def-use graph; they are all syntax-level
(tokens/trees). There is no CRAN-safe drop-in for `CodeDepends`. The prior
rejection stands.

**Use case B: use base `utils::getParseData()` + `utils::getParseText()`. Add no
dependency.** Tested and working. HDD's source-preserving subtree deletion is
achievable with zero new packages, provided the deletion routine is
sibling/operator-aware.

## Tool comparison

| Tool | CRAN | Non-CRAN deps | New CRAN pkgs | Fit for B |
|---|---|---|---|---|
| `utils::getParseData()` | base | 0 | **0** | **yes, tested** |
| `xmlparsedata` | yes | 0 | 1 | no gain over base |
| `treesitter` + `treesitter.r` | yes | — | **~8, one needs compilation** | works but disproportionate |
| `lintr` internals | yes | 11 imports | heavy | read-only, N/A |
| `styler` | yes | 8 | 8 | reformats to house style -- wrong goal |
| `CodeDepends` | yes | **`graph` (Bioconductor)** | — | disqualified (see other note) |
| base `parse()`/`as.list()` | base | 0 | 0 | no offsets -- insufficient alone |
| `renv` / `funspotr` / `flow` / `pkgapi` | mixed | — | — | wrong problem / off-CRAN |

`treesitter` gives a nicer node-edit API for the *same* algorithm, but costs ~8
packages and introduces native compilation to a package that currently has zero
compiled code -- a real CRAN-check/portability cost. Not worth it.

## Why getParseData() suffices (tested)

`getParseData()` returns, per token: `line1/col1/line2/col2` offsets, a `parent`
id, a terminal flag, and `text`. Every `expr` node's span is balanced by
construction, so deleting a full node span can never unbalance parens/braces.

### Tested cases

1. **Naive RHS-only deletion of a `%>%` stage** (delete `mutate(y = x*2)` but
   leave the preceding `%>%`) -> **hard parse error** `unexpected SPECIAL` on the
   dangling pipe. Fails loudly -- good.

2. **Operator-aware deletion** (delete from `end(LHS chain) + 1` through
   `end(mutate call)`, i.e. operator + operand together) ->
   ```r
   result <- df %>%
     filter(x > 0) %>%
     summarise(s = sum(y))
   ```
   Valid, indistinguishable from hand-edited source. **Confirmed for both `%>%`
   and native `|>`.**

3. **Call-argument comma hazard.** `f(a, b, c)`, delete `b` alone -> `f(a, , c)`.
   This **parses** (R permits missing arguments) but is semantically different
   and visually broken -- a **silent** failure, more dangerous than the pipe
   case. Delete `b` plus its trailing comma -> `f(a, c)`, correct.

### Failure-mode inventory (all confirmed by running R)

| Hazard | Behaviour | Mitigation |
|---|---|---|
| Dangling infix operator (`%>%`, `|>`, `+`) after RHS-only delete | hard parse error (loud) | delete operator + operand as one span |
| Double/trailing comma in arg list | **parses** -- silent semantic drift (most dangerous) | delete arg + adjacent comma atomically |
| Unbalanced parens/braces | only if deleting at wrong granularity | always delete a full `expr`/token span, never a text search |
| Blank-line / whitespace residue | readability nit, not correctness | tidy trailing newline; unverified at scale |

### Not tested (flagged honestly)

- Comments attached to a deleted subtree (trailing `# ...`) -- likely needs
  handling via the `COMMENT` token rows `getParseData()` returns; not verified.
- Multi-line bodies / nested braces / `if`/`for`/`{}` as deletion targets --
  only pipes and call args were exercised; the general "delete a full
  sibling-adjacent span" rule should generalise but was not tested.
- `treesitter`'s edit API -- assessed from CRAN metadata, not run.

## Design implication for 0.3.0

HDD deletion is **span-based on the original source text**, not deparse-based:

1. `getParseData(parse(text, keep.source = TRUE))` for the token tree with offsets.
2. Walk it level by level (HDD).
3. To delete a node, remove its character span **plus** its adjacent connective
   (the operator before a pipe stage, or the comma beside a call argument) as one
   atomic span, so no dangling operator or double comma survives.
4. Everything untouched keeps its exact original formatting -- solving the
   "minimal but still looks like my code" problem that `deparse()` cannot.

The critical rule: **never delete a bare node; always delete node + adjacent
connective together.** The comma case is the one that fails *silently*, so it
needs an explicit test, not just a "does it parse" check.

Zero new dependencies -- consistent with the package's posture and the
`CodeDepends` rejection.
