#' Minimize a failing R script to a reproducible example
#'
#' Reduces a failing piece of R code to a one-minimal subset of its top-level
#' statements that still triggers the same failure. The result is a *one-minimal*
#' example: removing any remaining statement makes the failure disappear. This is
#' the form requested when reporting bugs or asking for help, and the part of
#' preparing such an example that is usually done by hand.
#'
#' When no subset of the top-level statements can be removed -- the usual case
#' when the failure is nested inside a function body -- `minex()` continues
#' *within* the surviving statements instead of returning the input unchanged,
#' so the reduced code is then a simplification of the original statements
#' rather than a subset of them. See `granularity`.
#'
#' By default `minex()` first runs the whole input to record the failure it
#' produces (its condition message and class), then uses [ddmin()] to search for
#' a one-minimal subset that reproduces it. Each candidate is evaluated in a
#' separate R process so that dependencies between statements and their side
#' effects are respected; removing a statement that a later one needs typically
#' changes the error, and such a removal is therefore rejected.
#'
#' Supply a custom `oracle` to minimize against any condition you can express as
#' a predicate, rather than against the recorded failure. The oracle receives a
#' character vector of statements and must return a single logical. The empty
#' statement set is not considered; a custom oracle should be false for it if
#' the one-minimality guarantee is required.
#'
#' @param file Path to a file containing the R code to minimize. Used only when
#'   neither `code` nor `clipboard` is supplied.
#' @param code A character vector of R source lines, or a single string. Takes
#'   precedence over both `clipboard` and `file`.
#' @param clipboard Logical. If `TRUE`, read the R code to minimize from the
#'   system clipboard. Takes precedence over `file`, but is overridden by `code`.
#'   Input precedence is `code > clipboard > file`; supplying more than one
#'   source emits a warning and the highest-precedence source is used.
#' @param oracle Optional predicate taking a character vector of statements and
#'   returning a single logical. When supplied, the target failure is not
#'   recorded automatically and `condition`, `match` and failure-point truncation
#'   are all bypassed; you are fully responsible for defining what counts as
#'   reproducing the failure. Empty candidates are not tested.
#' @param condition Which kind of condition to target: `"error"` (the default),
#'   `"warning"`, `"message"`, or `"any"` (the most severe condition present,
#'   error > warning > message). Ignored when a custom `oracle` is supplied.
#' @param match How a candidate's failure must match the recorded one when no
#'   `oracle` is given: `"message"` (identical message, the default), `"class"`
#'   (shares a specific condition class) or `"both"`. Class matching excludes
#'   generic classes such as `"error"`, `"condition"`, and `"simpleError"`.
#'   For targets with only generic classes, it requires the same message and a
#'   compatible generic class when available. `"both"` also requires the same message
#'   for specific-class targets. Matching on the message is usually
#'   right, because an over-reduced fragment tends to fail with a different
#'   message (for example "object not found"). Can also be a function taking
#'   two condition summaries, `candidate` and `target` (each a list with
#'   `message` and `classes`), and returning a single logical, for custom
#'   matching logic.
#' @param algorithm The reduction strategy passed to [ddmin()]: `"cdd"`
#'   (the default, adapted from Counter-Based Delta Debugging) or `"ddmin"` (the
#'   classic block-halving loop).
#' @param backend Either `"callr"` (evaluate each candidate in a fresh R process,
#'   the default and the only choice that fully isolates state) or `"inprocess"`
#'   (evaluate in the current session, faster but without isolation; a script's
#'   side effects other than options are not sandboxed). **Soundness caveat:**
#'   `"inprocess"` evaluates in an environment chained to the caller's global
#'   environment, so a candidate that references a name which happens to exist
#'   in the caller's workspace resolves it instead of raising `object not
#'   found`. Over-reduction can therefore spuriously still "succeed" against
#'   that tripwire. `backend = "callr"` (the default) evaluates in a clean
#'   process and is unaffected; prefer `"inprocess"` only for trusted, quick
#'   iteration.
#' @param timeout Maximum seconds allowed for a single `callr` evaluation.
#' @param max_oracle_calls Numeric scalar, a whole number at least 1 or `Inf`.
#'   Upper bound on actual oracle evaluations across the whole reduction search,
#'   including statement reduction, expression reduction, and verification.
#'   Cache hits and parse rejects do not consume the budget. When it is exhausted the
#'   reduction stops early with a warning; the result still reproduces the
#'   failure but may not be one-minimal. Initial target observation is setup,
#'   excluded from this budget and from `oracle_calls`; its confirmed source is
#'   reused rather than evaluated again. The one-time failure-point truncation
#'   probe is also exempt from the budget, but is included in `oracle_calls`.
#'   Thus the reported count can exceed the search budget by one. When the
#'   statement pass escalates to `"expression"` (see `granularity`), the second
#'   pass runs on whatever the first left rather than on a fresh budget.
#' @param granularity `"statement"` (the default) reduces only at the level of
#'   top-level statements. `"expression"`
#'   additionally reduces *within* each surviving statement via a specialized
#'   HDD*-style pass based on hierarchical delta debugging: pipeline stages
#'   (`|>`, magrittr `%>%`) and positional call arguments can be dropped from a
#'   statement as long as the whole kept set still reproduces the target
#'   failure. Calls containing missing argument slots are left intact; nested
#'   calls with complete argument lists can still be reduced. This pass
#'   does not reduce every R syntax-tree node. When
#'   such a reduction makes an earlier statement redundant (for example a value
#'   dropped from a later call), that statement is swept out, so the result stays
#'   statement one-minimal.
#'
#'   When `granularity` is left at its default *and* statement-level reduction
#'   removes nothing, `minex()` retries once at `"expression"` rather than
#'   returning the input unchanged. This is the common case for a script that is
#'   one function definition plus a call, where every top-level statement is
#'   load-bearing but the failure is nested inside the function body. The
#'   retried result carries `escalated_from = "statement"`, and its `code` is a
#'   simplification of the original statements rather than a subset of them.
#'   Passing `granularity = "statement"` explicitly suppresses the retry and
#'   reduces at statement level only. No retry happens when the statement pass
#'   stopped early against `max_oracle_calls`, since it has not then shown that
#'   nothing is removable.
#' @param verbose Logical. If `TRUE`, report progress. If `"trace"`, also
#'   populate the result's `trace`. Call IDs count actual predicate evaluations
#'   globally across phases and automatic escalation. Cached rows retain the
#'   current ID; reuse of the initial target can therefore have ID 0. Rows with
#'   `cached = FALSE` and an empty `event` correspond to actual evaluations.
#'   Phases are `"truncate"`, `"reduce"`, `"verify"`, `"verify_empty"`,
#'   `"joint_verify"`, and `"post_hdd_verify"`; events include
#'   `"budget_exhausted"` and `"parse_rejected"`. For
#'   `granularity = "expression"`, the HDD trace rows additionally carry `stmt_index`
#'   (which statement they reduced) and `level` (the HDD tree depth); the
#'   statement-level rows have `NA` in both. For `granularity = "statement"`
#'   the trace has no `stmt_index`/`level` columns.
#'
#' @return An object of class `"minex_result"`: a list with the minimized `code`
#'   (a character vector of statements), the `original` statements, the statement
#'   counts `n_original` and `n_minimal`, the character counts `n_chars_original`
#'   and `n_chars_minimal` (`nchar()` of the code collapsed to a single string,
#'   before and after reduction), the number of `oracle_calls` (every predicate
#'   evaluation, including the failure-point truncation probe), the recorded
#'   `target` failure (or `NULL` for a custom oracle), the `granularity` setting
#'   used, and the `match` and `backend` settings. `complete` is `FALSE` when the
#'   search exhausts its budget or a candidate evaluation cannot be observed.
#'   `evaluation_failures` lists unobserved candidates, each with its `code` and
#'   a `failure` containing the backend failure type, message, classes, and
#'   original condition. Unobserved candidates are retained conservatively;
#'   they do not establish one-minimality. If the input or a required full-set
#'   baseline cannot be observed, a `minex_evaluation_error` is raised instead,
#'   with the backend condition available as its `parent`.
#'
#'   When the statement pass escalated to `"expression"`, the result also carries
#'   `escalated_from = "statement"` and `coarse_oracle_calls`, the share of
#'   `oracle_calls` spent on the initial statement-level pass. Both are absent
#'   otherwise, so `is.null(res$escalated_from)` distinguishes a result that was
#'   reduced at the granularity asked for from one that had to descend.
#'
#' @references
#' Zhang M, Xu Z, Tian Y, Cheng X, Sun C (2025). "Toward a Better Understanding
#' of Probabilistic Delta Debugging." *ICSE 2025*.
#' \doi{10.1109/ICSE55347.2025.00117}
#'
#' Misherghi G, Su Z (2006). "HDD: Hierarchical Delta Debugging." *ICSE 2006*,
#' 142-151. \doi{10.1145/1134285.1134307}
#'
#' For related work on repeatedly invoking DDMIN to a fixed point, see Vince D,
#' Kiss A (2024). "Evaluation of the Fixed-Point Iteration of Minimizing Delta
#' Debugging." *Journal of Software: Evolution and Process*, 36(10), e2702.
#' \doi{10.1002/smr.2702} The verification sweep used here instead repeatedly
#' tests singleton deletions; it is not the paper's DDMIN* algorithm.
#'
#' @seealso [ddmin()] for the underlying algorithm and [reduce_rows()] for
#'   reducing data frames.
#'
#' @examples
#' # A failing script padded with irrelevant setup.
#' script <- c(
#'   "a <- 10",
#'   "b <- 20",
#'   "log('not a number')"
#' )
#' res <- minex(code = script, backend = "inprocess")
#' res
#' cat(as.character(res), "\n")
#'
#' # A failure that genuinely depends on an earlier statement: both are kept.
#' script2 <- c(
#'   "x <- c(1, 2, NA)",
#'   "m <- mean(x)",
#'   "if (is.na(m)) stop('mean is NA')"
#' )
#' minex(code = script2, backend = "inprocess")
#'
#' \donttest{
#' # The default backend runs candidates in fresh R processes.
#' minex(code = script)
#' }
#'
#' \dontrun{
#' # Reduce a failing script copied to the system clipboard:
#' minex(clipboard = TRUE)
#' }
#' @export
minex <- function(file = NULL,
                  code = NULL,
                  clipboard = FALSE,
                  oracle = NULL,
                  condition = c("error", "warning", "message", "any"),
                  match = c("message", "class", "both"),
                  algorithm = c("cdd", "ddmin"),
                  backend = c("callr", "inprocess"),
                  timeout = 60,
                  max_oracle_calls = Inf,
                  verbose = FALSE,
                  granularity = c("statement", "expression")) {
  condition <- match.arg(condition)
  if (!is.function(match)) match <- match.arg(match)
  algorithm <- match.arg(algorithm)
  backend   <- match.arg(backend)
  validate_oracle_budget(max_oracle_calls)
  # Recorded before match.arg() overwrites the argument, so an explicitly
  # requested granularity is never silently overridden.
  .auto_escalate <- missing(granularity)
  granularity <- match.arg(granularity)

  # Input precedence: code > clipboard > file; warn on multi-source.
  sources <- c(code = !is.null(code), clipboard = isTRUE(clipboard),
               file = !is.null(file))
  if (sum(sources) > 1L) {
    warning("Multiple input sources supplied; using ",
            names(sources)[which(sources)[1]], " (code > clipboard > file).",
            call. = FALSE)
  }
  if (!is.null(code)) {
    # use code as-is
  } else if (isTRUE(clipboard)) {
    code <- read_clipboard()
  } else if (!is.null(file)) {
    if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
    code <- readLines(file, warn = FALSE)
  } else {
    stop("Supply `code`, `file`, or `clipboard = TRUE`.", call. = FALSE)
  }

  statements <- split_statements(code)
  if (length(statements) < 1L) {
    stop("No parseable R statements were found in the input.", call. = FALSE)
  }
  statements_full <- statements   # pre-truncation, for honest `original`/`n_original`

  target <- NULL
  evaluation_failures <- list() # unobserved candidates are not negative evidence
  context <- new_oracle_context(max_oracle_calls, verbose)
  context$owner <- "minex"
  if (is.null(oracle)) {
    matcher <- build_matcher(match)
    full <- run_code(statements, backend = backend, timeout = timeout)
    if (!isTRUE(full$observed)) {
      stop_unobserved_evaluation(full, backend, "input")
    }
    # Choose the target condition, then its defining statement index.
    target_cond <- pick_target_condition(full$conditions, condition)
    if (is.null(target_cond)) {
      stop(sprintf(
        "The input produced no %s; nothing to minimize.", condition),
        call. = FALSE)
    }
    idx <- pick_target_index(full$conditions, target_cond, matcher, condition)
    target <- list(message = target_cond$message, classes = target_cond$classes,
                   conditions = full$conditions, failing_index = idx)
    oracle <- oracle_wrap(function(stmts) {
      result <- run_code(stmts, backend = backend, timeout = timeout)
      if (!isTRUE(result$observed)) {
        # Without an observed baseline the reducer cannot even confirm its
        # precondition. Other unknown candidates leave confirmed progress intact.
        if (identical(stmts, statements)) {
          stop_unobserved_evaluation(result, backend, "full-set baseline")
        }
        evaluation_failures[[length(evaluation_failures) + 1L]] <<-
          list(code = stmts, failure = result$failure)
        return(FALSE)
      }
      cand <- pick_target_condition(result$conditions, condition)
      if (is.null(cand)) return(FALSE)
      isTRUE(matcher(cand, target_cond))
    }, context)
    if (isTRUE(matcher(target_cond, target_cond))) {
      assign(oracle_key(statements), TRUE, envir = context$cache)
    }
    # Free failure-point truncation, with a fallback for flaky scripts: only
    # keep the truncated set if it still reproduces (spec section 2). This is one
    # real oracle evaluation: counted in oracle_calls but exempt from the search
    # budget. Its result is cached for the following precondition and search.
    if (!is.na(idx)) {
      truncated <- truncate_statements(statements, idx)
      if (length(truncated) < length(statements)) {
        oracle_metadata(context, "truncate", length(truncated))
        if (isTRUE(oracle(truncated, .exempt = TRUE))) {
          statements <- truncated
        }
      }
    }
  } else if (!is.function(oracle)) {
    stop("`oracle` must be a function.", call. = FALSE)
  } else {
    oracle <- oracle_wrap(oracle, context)
  }

  info <- new.env()
  oracle_metadata(context, "reduce", length(statements))
  minimal <- ddmin(statements, oracle,
                   algorithm = algorithm, max_oracle_calls = max_oracle_calls,
                   verbose = verbose, .info = info)

  code_out <- minimal
  complete <- isTRUE(info$complete)
  jointly_failed <- FALSE                    # per-statement reductions didn't compose
  escalated <- FALSE
  coarse_calls <- NULL
  # Descend within the same context: keep the original target, confirmed source
  # configurations, diagnostics, budget, and trace from the statement pass.
  if (.auto_escalate && granularity == "statement" && complete &&
      !length(evaluation_failures) &&
      length(minimal) >= length(statements_full) &&
      context$budget_calls < context$max_calls) {
    granularity <- "expression"
    escalated <- TRUE
    coarse_calls <- context$calls
  }
  if (granularity == "expression") {
    for (i in seq_along(minimal)) {
      # reproduce(cand_text): replace statement i with the candidate, run the
      # whole kept set through the existing oracle (run_code + matcher).
      reproduces <- oracle_inherit(local({
        idx_i <- i
        function(cand_text) {
          cand_set <- minimal
          cand_set[idx_i] <- cand_text
          isTRUE(oracle(cand_set))
        }
      }), context)
      sub <- new.env()
      oracle_metadata(context, "reduce", length(minimal), stmt_index = i)
      code_out[i] <- hdd_star(minimal[i], reproduces, info = sub,
                              verbose = verbose)
      if (!isTRUE(sub$complete)) complete <- FALSE
    }

    # Each statement was reduced against the OTHERS held at their un-reduced
    # (`minimal`) form, so independently-valid per-statement reductions can
    # still fail to reproduce when combined. Joint-verify the assembled
    # `code_out` before trusting it; fall back to the known-good `minimal`
    # when the composition doesn't hold or can't be confirmed.
    if (!identical(code_out, minimal)) {
      oracle_metadata(context, "joint_verify", length(code_out))
      unknown_before <- length(evaluation_failures)
      joint <- tryCatch(oracle(code_out), minex_oracle_limit_reached = function(e) NA)
      if (!isTRUE(joint)) {
        code_out <- minimal
        jointly_failed <- !is.na(joint) && length(evaluation_failures) == unknown_before
        complete <- FALSE
      } else {
        # HDD can make earlier statements redundant. Re-sweep to a fixpoint,
        # sharing the same source cache, budget, and confirmation guarantees.
        repeat {
          removed_any <- FALSE
          i <- 1L
          while (i <= length(code_out) && length(code_out) > 1L) {
            oracle_metadata(context, "post_hdd_verify", length(code_out) - 1L)
            removable <- tryCatch(oracle(code_out[-i]),
              minex_oracle_limit_reached = function(e) NA)
            if (is.na(removable)) { complete <- FALSE; break }
            if (isTRUE(removable)) {
              code_out <- code_out[-i]   # element now at i is new; do not advance
              removed_any <- TRUE
            } else {
              i <- i + 1L
            }
          }
          if (!removed_any || is.na(removable)) break
        }
      }
    }
  }

  if (length(evaluation_failures)) {
    complete <- FALSE
    warning(sprintf(
      "%d candidate evaluation(s) could not be observed; returning confirmed code, ",
      length(evaluation_failures)),
      "but the result may still be reducible. Inspect `evaluation_failures` ",
      "and retry after resolving the backend failure or increasing `timeout`. ",
      "First failure: ", evaluation_failures[[1L]]$failure$message,
      call. = FALSE)
  }
  if (jointly_failed) {
    warning("Sub-expression reductions did not jointly reproduce the failure; ",
            "returning the statement-level result.", call. = FALSE)
  } else if (!complete && !length(evaluation_failures)) {
    warning("minex stopped early; result reproduces the failure but may still ",
            "be reducible. Re-run with a higher `max_oracle_calls`.",
            call. = FALSE)
  }

  trace_out <- build_trace(context$rows)
  if (granularity == "statement" && !is.null(trace_out)) {
    trace_out <- trace_out[c("call", "phase", "size", "kept", "cached", "event")]
  }

  .result <- structure(
    list(code = code_out, original = statements_full,
         n_original = length(statements_full), n_minimal = length(code_out),
         oracle_calls = context$calls, target = target, match = match,
         backend = backend, complete = complete, algorithm = algorithm,
         evaluation_failures = evaluation_failures,
         condition = condition, max_oracle_calls = max_oracle_calls,
         trace = trace_out, granularity = granularity,
         n_chars_original = nchar(paste(statements_full, collapse = "\n")),
         n_chars_minimal = nchar(paste(code_out, collapse = "\n"))),
    class = "minex_result"
  )

  if (escalated) {
    .result$escalated_from <- "statement"
    .result$coarse_oracle_calls <- coarse_calls
  }
  .result
}
