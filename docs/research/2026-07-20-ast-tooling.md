# AST tooling for HDD source reconstruction: research notes

Date: 2026-07-20
Status: historical design notes updated to match the 0.2.0 implementation

## Question

Is there a CRAN-safe replacement for `CodeDepends`, for two use cases?

- **A (near-term):** def-use dependency analysis over top-level statements.
- **B (implemented in 0.2.0):** parse-tree traversal for an HDD*-style pass -- deleting
  subtrees and regenerating **valid, human-readable** R source. Hard part is
  source reconstruction, since a mutated tree has no `srcref`.

## Verdicts

**Use case A: verdict unchanged -- do not build / hand-roll if ever revisited.**
No implementation or comparative tool evaluation is retained. The conservative
design concerns are recorded in the static-prefiltering note.

**Use case B: use base `utils::getParseData()`. Add no dependency.** The shipped
implementation uses parse-data offsets and connective-aware deletion spans for
pipeline stages and positional call arguments.

## Why getParseData() suffices

`getParseData()` returns, per token: `line1/col1/line2/col2` offsets, a `parent`
id, a terminal flag, and `text`. The implementation uses those offsets to delete
an operand together with the adjacent pipe operator or comma, then reparses each
candidate before invoking the oracle.

## Preserved regression evidence

The repository retains the relevant cases in
`tests/testthat/test-parse-tree.R` and `tests/testthat/test-hdd.R`, including:

- native and magrittr pipe recognition and native-pipe stage deletion;
- deletion of positional arguments in every position;
- grouping, control-flow, namespaced, and method-style call handling;
- seam-local whitespace cleanup and trailing-comment guards; and
- fixed-point reduction of supported spans.

The implementation does not claim general deletion of multi-line bodies,
conditionals, loops, named arguments, or arbitrary R syntax-tree nodes.

## Implemented design

HDD deletion is **span-based on the original source text**, not deparse-based:

1. `getParseData(parse(text, keep.source = TRUE))` for the token tree with offsets.
2. Group supported deletion spans by depth (an HDD-style traversal).
3. To delete a node, remove its character span **plus** its adjacent connective
   (the operator before a pipe stage, or the comma beside a call argument) as one
   atomic span, so no dangling operator or double comma survives.
4. Everything untouched keeps its exact original formatting -- solving the
   "minimal but still looks like my code" problem that `deparse()` cannot.

The critical rule is to delete a supported node span together with its adjacent
connective and then reparse the candidate. This is a specialized HDD*-style
pass, not a complete implementation of general HDD over the R syntax tree.

Zero new dependencies -- consistent with the package's posture and the
`CodeDepends` rejection.
