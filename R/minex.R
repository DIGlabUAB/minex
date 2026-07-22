#' Minimize a failing R script to a reproducible example
#'
#' Reduces a failing piece of R code to the smallest subset of its top-level
#' statements that still triggers the same failure. The result is a *one-minimal*
#' example: removing any remaining statement makes the failure disappear. This is
#' the form requested when reporting bugs or asking for help, and the part of
#' preparing such an example that is usually done by hand.
#'
#' By default `minex()` first runs the whole input to record the failure it
#' produces (its condition message and class), then uses [ddmin()] to search for
#' a minimal subset that reproduces it. Each candidate is evaluated in a separate
#' R process so that dependencies between statements and their side effects are
#' respected; removing a statement that a later one needs typically changes the
#' error, and such a removal is therefore rejected.
#'
#' Supply a custom `oracle` to minimize against any condition you can express as
#' a predicate, rather than against the recorded failure. The oracle receives a
#' character vector of statements and must return a single logical.
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
#'   reproducing the failure.
#' @param condition Which kind of condition to target: `"error"` (the default),
#'   `"warning"`, `"message"`, or `"any"` (the most severe condition present,
#'   error > warning > message). Ignored when a custom `oracle` is supplied.
#' @param match How a candidate's failure must match the recorded one when no
#'   `oracle` is given: `"message"` (identical message, the default), `"class"`
#'   (shares a condition class) or `"both"`. Matching on the message is usually
#'   right, because an over-reduced fragment tends to fail with a different
#'   message (for example "object not found"). Can also be a function taking
#'   two condition summaries, `candidate` and `target` (each a list with
#'   `message` and `classes`), and returning a single logical, for custom
#'   matching logic.
#' @param algorithm The reduction strategy passed to [ddmin()]: `"cdd"`
#'   (convergent delta debugging, the default) or `"ddmin"` (the classic
#'   block-halving loop).
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
#' @param max_oracle_calls Numeric upper bound on the number of oracle
#'   evaluations in the reduction search, passed to [ddmin()] (and to the HDD
#'   pass when `granularity = "expression"`). When the budget is exhausted the
#'   reduction stops early with a warning; the result still reproduces the
#'   failure but may not be one-minimal. The one-time failure-point truncation
#'   probe is mandatory setup, exempt from this limit, but is still included in
#'   the reported `oracle_calls`.
#' @param granularity `"statement"` (the default) reduces only at the level of
#'   top-level statements, matching 0.2.0 behavior exactly. `"expression"`
#'   additionally reduces *within* each surviving statement via HDD (hierarchical
#'   delta debugging): pipeline stages (`|>`, magrittr `%>%`) and positional call
#'   arguments can be dropped from a statement as long as the whole kept set
#'   still reproduces the target failure. When such a reduction makes an earlier
#'   statement redundant (for example a value dropped from a later call), that
#'   statement is swept out, so the result stays statement one-minimal.
#' @param verbose Logical. If `TRUE`, report progress. If `"trace"`, also
#'   populate the result's `trace` with a per-oracle-call record. For
#'   `granularity = "expression"`, the HDD trace rows additionally carry `stmt_index`
#'   (which statement they reduced) and `level` (the HDD tree depth); the
#'   statement-level rows have `NA` in both. For `granularity = "statement"`
#'   the trace is unchanged from 0.2.0 (no `stmt_index`/`level` columns).
#'
#' @return An object of class `"minex_result"`: a list with the minimized `code`
#'   (a character vector of statements), the `original` statements, the statement
#'   counts `n_original` and `n_minimal`, the character counts `n_chars_original`
#'   and `n_chars_minimal` (`nchar()` of the code collapsed to a single string,
#'   before and after reduction), the number of `oracle_calls` (every predicate
#'   evaluation, including the failure-point truncation probe), the recorded
#'   `target` failure (or `NULL` for a custom oracle), the `granularity` setting
#'   used, and the `match` and `backend` settings.
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
  oracle_calls_probe <- 0L   # the failure-point truncation probe, if it runs
  if (is.null(oracle)) {
    matcher <- build_matcher(match)
    full <- run_code(statements, backend = backend, timeout = timeout)
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
    oracle <- function(stmts) {
      result <- run_code(stmts, backend = backend, timeout = timeout)
      cand <- pick_target_condition(result$conditions, condition)
      if (is.null(cand)) return(FALSE)
      isTRUE(matcher(cand, target_cond))
    }
    # Free failure-point truncation, with a fallback for flaky scripts: only
    # keep the truncated set if it still reproduces (spec section 2). This is one
    # real oracle evaluation: it is counted in the reported oracle_calls but, like
    # ddmin's precondition, is mandatory setup and exempt from max_oracle_calls.
    if (!is.na(idx)) {
      truncated <- truncate_statements(statements, idx)
      if (length(truncated) < length(statements)) {
        oracle_calls_probe <- 1L
        if (isTRUE(oracle(truncated))) {
          statements <- truncated
        }
      }
    }
  } else if (!is.function(oracle)) {
    stop("`oracle` must be a function.", call. = FALSE)
  }

  info <- new.env()
  minimal <- ddmin(statements, function(s) isTRUE(oracle(s)),
                   algorithm = algorithm, max_oracle_calls = max_oracle_calls,
                   verbose = verbose, .info = info)

  code_out <- minimal
  running  <- (info$oracle_calls %||% 0L) + oracle_calls_probe  # shared counter (probe + stmt-level + HDD)
  complete <- isTRUE(info$complete)
  jointly_failed <- FALSE                    # per-statement reductions didn't compose
  hdd_trace_parts <- list()                  # per-statement HDD traces, tagged with stmt_index
  if (granularity == "expression") {
    for (i in seq_along(minimal)) {
      # reproduce(cand_text): replace statement i with the candidate, run the
      # whole kept set through the existing oracle (run_code + matcher).
      reproduces <- local({
        idx_i <- i
        function(cand_text) {
          cand_set <- minimal
          cand_set[idx_i] <- cand_text
          isTRUE(oracle(cand_set))
        }
      })
      hoc <- hdd_make_oracle(reproduces)     # source-string memo (Task 6)
      sub <- new.env()
      remaining <- if (is.finite(max_oracle_calls)) max_oracle_calls - running else Inf
      if (is.finite(remaining) && remaining < 1) { complete <- FALSE; break }
      code_out[i] <- hdd_star(minimal[i], hoc, max_oracle_calls = remaining, info = sub,
                              verbose = verbose)
      running <- running + (sub$oracle_calls %||% 0L)
      if (!isTRUE(sub$complete)) complete <- FALSE
      if (wants_trace(verbose) && !is.null(sub$trace)) {
        tagged <- sub$trace
        tagged$stmt_index <- i
        hdd_trace_parts[[length(hdd_trace_parts) + 1L]] <- tagged
      }
    }

    # Each statement was reduced against the OTHERS held at their un-reduced
    # (`minimal`) form, so independently-valid per-statement reductions can
    # still fail to reproduce when combined. Joint-verify the assembled
    # `code_out` before trusting it; fall back to the known-good `minimal`
    # when the composition doesn't hold or can't be confirmed.
    if (!identical(code_out, minimal)) {
      remaining <- if (is.finite(max_oracle_calls)) max_oracle_calls - running else Inf
      if (is.finite(remaining) && remaining < 1) {
        # Can't afford the verify call: unconfirmed, so don't trust code_out.
        code_out <- minimal
        complete <- FALSE
      } else {
        running <- running + 1L
        if (!isTRUE(oracle(code_out))) {
          code_out <- minimal
          jointly_failed <- TRUE
          complete <- FALSE
        } else {
          # code_out is confirmed to reproduce. A statement HDD stripped down
          # (e.g. an argument dropped from a later call) can now be redundant, so
          # the statement set may no longer be one-minimal. Re-sweep to a
          # fixpoint: drop any statement whose removal still reproduces. Sound --
          # each drop is oracle-confirmed first; the calls are budgeted exactly
          # like the joint-verify above.
          repeat {
            removed_any <- FALSE
            i <- 1L
            while (i <= length(code_out) && length(code_out) > 1L) {
              remaining <- if (is.finite(max_oracle_calls)) max_oracle_calls - running else Inf
              if (is.finite(remaining) && remaining < 1) { complete <- FALSE; break }
              running <- running + 1L
              if (isTRUE(oracle(code_out[-i]))) {
                code_out <- code_out[-i]   # element now at i is new; do not advance
                removed_any <- TRUE
              } else {
                i <- i + 1L
              }
            }
            remaining <- if (is.finite(max_oracle_calls)) max_oracle_calls - running else Inf
            if (!removed_any || (is.finite(remaining) && remaining < 1)) break
          }
        }
      }
    }
  }

  if (jointly_failed) {
    warning("Sub-expression reductions did not jointly reproduce the failure; ",
            "returning the statement-level result.", call. = FALSE)
  } else if (!complete) {
    warning("minex stopped early; result reproduces the failure but may still ",
            "be reducible. Re-run with a higher `max_oracle_calls`.",
            call. = FALSE)
  }

  # Trace assembly. Statement-granularity (or non-"trace" verbose) keeps the
  # `call,phase,size,kept,cached,event` shape byte-for-byte (L14): only when
  # HDD actually ran under verbose = "trace" do stmt_index/level get added.
  if (granularity == "expression" && wants_trace(verbose)) {
    stmt_trace <- info$trace
    if (!is.null(stmt_trace)) {
      stmt_trace$stmt_index <- NA_integer_
      stmt_trace$level <- NA_integer_
    }
    hdd_trace <- if (length(hdd_trace_parts) == 0L) {
      NULL
    } else {
      do.call(rbind, hdd_trace_parts)
    }
    trace_cols <- c("call", "phase", "size", "kept", "cached", "event",
                    "stmt_index", "level")
    trace_pieces <- Filter(Negate(is.null), list(stmt_trace, hdd_trace))
    trace_out <- if (length(trace_pieces) == 0L) {
      NULL
    } else {
      do.call(rbind, lapply(trace_pieces, `[`, trace_cols))
    }
  } else {
    trace_out <- info$trace
  }

  structure(
    list(code = code_out, original = statements_full,
         n_original = length(statements_full), n_minimal = length(code_out),
         oracle_calls = running, target = target, match = match,
         backend = backend, complete = complete, algorithm = algorithm,
         condition = condition, max_oracle_calls = max_oracle_calls,
         trace = trace_out, granularity = granularity,
         n_chars_original = nchar(paste(statements_full, collapse = "\n")),
         n_chars_minimal = nchar(paste(code_out, collapse = "\n"))),
    class = "minex_result"
  )
}
