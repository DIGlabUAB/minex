#' Minimize a collection with delta debugging
#'
#' A general-purpose delta debugging interface with two first-phase strategies:
#' the default adaptation of Counter-Based Delta Debugging and the classic
#' `ddmin` algorithm of Zeller and Hildebrandt (2002). Given a collection of
#' elements and a predicate that reports whether a subset still exhibits some
#' behavior of interest, `ddmin()` returns a subset that is *one-minimal*: the
#' predicate holds for it, but fails for every subset obtained by removing a
#' single element. This guarantee assumes the predicate is false for the empty
#' subset, which `ddmin()` does not test. One-minimality does not imply a
#' globally smallest subset.
#'
#' The classic strategy partitions the current candidate into `n` blocks
#' (starting with `n = 2`). It tests individual blocks and then their
#' complements, increasing granularity when neither reduces the candidate. The
#' CDD adaptation instead uses the cited counter-based block-size equations and
#' restarts after successful deletions. Both are followed by the repeated
#' singleton-deletion sweep described under `algorithm`.
#'
#' Results of the predicate are cached on the set of element indices, so an
#' identical configuration is never evaluated twice.
#'
#' @param items A list or atomic vector of elements to minimize.
#' @param interesting A predicate applied to a subset of `items`, in the same
#'   form as `items`, returning a single logical. It should return `TRUE` when
#'   the subset still reproduces the behavior of interest. Empty subsets are not
#'   tested; this predicate should be false for the empty subset if the
#'   one-minimality guarantee is required.
#' @param algorithm Character. The reduction strategy for the first phase, one
#'   of `"cdd"` (the default, adapted from Counter-Based Delta Debugging) or
#'   `"ddmin"` (the classic block-halving loop). The CDD adaptation restarts its
#'   round counter after a successful deletion. Both strategies are followed by
#'   a fixed-point verification sweep that removes any remaining removable
#'   single elements.
#' @param max_oracle_calls Numeric. An upper bound on the number of predicate
#'   evaluations, including the full-set precondition test. Must be a numeric
#'   scalar, a whole number at least 1 or `Inf`. Cache hits do not consume it.
#'   When the budget is exhausted the reduction
#'   stops early and returns the smallest set confirmed so far; the result is
#'   still guaranteed to reproduce the behavior but may not be one-minimal.
#' @param verbose Logical or character. If `TRUE`, report each reduction phase
#'   via [message()]. If `"trace"`, additionally record a per-call trace in
#'   `.info$trace`.
#' @param .info Optional environment. When supplied, `ddmin()` populates it with
#'   `oracle_calls` (the number of predicate evaluations), `complete` (whether
#'   the reduction finished within budget), and `trace` (a data frame when
#'   `verbose = "trace"`, otherwise `NULL`). Metadata flows only through this
#'   environment; the returned value itself carries no attributes.
#'
#' @return The one-minimal subset of `items`, in the original order.
#'
#' @references
#' Zeller A, Hildebrandt R (2002). "Simplifying and Isolating Failure-Inducing
#' Input." *IEEE Transactions on Software Engineering*, 28(2), 183-200.
#' \doi{10.1109/32.988498}
#'
#' Zhang M, Xu Z, Tian Y, Cheng X, Sun C (2025). "Toward a Better Understanding
#' of Probabilistic Delta Debugging." *ICSE 2025*.
#' \doi{10.1109/ICSE55347.2025.00117}
#'
#' For related work on repeatedly invoking DDMIN to a fixed point, see Vince D,
#' Kiss A (2024). "Evaluation of the Fixed-Point Iteration of Minimizing Delta
#' Debugging." *Journal of Software: Evolution and Process*, 36(10), e2702.
#' \doi{10.1002/smr.2702} The verification sweep used here instead repeatedly
#' tests singleton deletions; it is not the paper's DDMIN* algorithm.
#'
#' @seealso [minex()] for the script-reduction front end and [reduce_rows()] for
#'   reducing data frames.
#'
#' @examples
#' # Reduce a sentence to the single word a predicate depends on.
#' words <- strsplit("the quick brown fox", " ")[[1]]
#' ddmin(words, function(s) "fox" %in% s)
#'
#' # When several elements are jointly required, all of them are kept.
#' nums <- 1:6
#' ddmin(nums, function(s) sum(s) >= 11 && 6 %in% s)
#' @export
ddmin <- function(items, interesting,
                  algorithm = c("cdd", "ddmin"),
                  max_oracle_calls = Inf,
                  verbose = FALSE,
                  .info = NULL) {
  if (!is.function(interesting)) {
    stop("`interesting` must be a function.", call. = FALSE)
  }
  algorithm <- match.arg(algorithm)
  validate_oracle_budget(max_oracle_calls)
  n_items <- length(items)
  if (n_items == 0L) {
    if (is.environment(.info)) {
      .info$oracle_calls <- 0L; .info$complete <- TRUE; .info$trace <- NULL
    }
    return(items)
  }

  context <- oracle_context(interesting)
  shared <- !is.null(context)
  if (!shared) context <- new_oracle_context(max_oracle_calls, verbose)
  calls_before <- context$calls
  trace_before <- length(context$rows)
  cache <- new.env(parent = emptyenv())
  complete <- TRUE
  phase <- "reduce"       # updated to "verify" before the sweep
  say <- function(...) if (verbose_enabled(verbose)) message("minex: ", ...)

  test <- function(idx) {
    idx <- sort(unique(idx))
    key <- paste(idx, collapse = ",")
    context$phase <- phase
    context$size <- length(idx)
    cached <- cache[[key]]
    if (!is.null(cached)) {
      oracle_trace(context, cached, cached = TRUE)
      return(cached)
    }
    result <- if (shared) {
      isTRUE(interesting(items[idx]))
    } else {
      oracle_test(context, key, function() interesting(items[idx]))
    }
    assign(key, result, envir = cache)
    result
  }

  if (!test(seq_len(n_items))) {
    stop("`interesting` is FALSE for the full set; nothing to minimize.",
         call. = FALSE)
  }

  reduce_fn <- if (algorithm == "cdd") cdd_reduce else ddmin_classic
  kept <- seq_len(n_items)
  # A budget-exhausted call unwinds out of the reducer via a condition, discarding
  # its stack frame. The reducers checkpoint each CONFIRMED reduction here so the
  # smallest set proven so far survives the unwind (docs promise this, not the
  # full input). `progress$kept` only ever holds a set that tested TRUE.
  progress <- new.env(parent = emptyenv())
  progress$kept <- kept
  tryCatch(
    {
      say("reduce phase (", algorithm, "), ", length(kept), " items")
      kept <- reduce_fn(kept, test, progress)
      phase <- "verify"
      say("verify phase, ", length(kept), " items")
      kept <- remove_removable_singles(kept, test, progress)
    },
    minex_oracle_limit_reached = function(e) {
      complete <<- FALSE
      kept <<- progress$kept   # recover the smallest confirmed set
      say("oracle-call limit reached; result may still be reducible")
    }
  )

  if (is.environment(.info)) {
    .info$oracle_calls <- context$calls - calls_before
    .info$complete <- complete
    .info$trace <- if (length(context$rows) > trace_before) {
      build_trace(context$rows[seq.int(trace_before + 1L, length(context$rows))])
    } else NULL
    if (!shared && !is.null(.info$trace)) {
      .info$trace <- .info$trace[c("call", "phase", "size", "kept", "cached", "event")]
    }
  }
  items[kept]
}

#' @keywords internal
#' @noRd
ddmin_classic <- function(kept, test, progress = NULL) {
  n <- 2L
  repeat {
    len <- length(kept)
    if (len < 2L) break
    groups <- as.integer(cut(seq_len(len), breaks = min(n, len), labels = FALSE))
    blocks <- unname(split(kept, groups))
    hit <- Find(function(b) test(b), blocks)
    if (!is.null(hit)) { kept <- hit; checkpoint(progress, kept); n <- 2L; next }
    complements <- lapply(blocks, function(b) setdiff(kept, b))
    hit <- Find(function(b) length(b) > 0L && test(b), complements)
    if (!is.null(hit)) { kept <- hit; checkpoint(progress, kept); n <- max(n - 1L, 2L); next }
    if (n >= len) break
    n <- min(2L * n, len)
  }
  kept
}

# Record a confirmed reduction so it survives a budget-exhaustion unwind.
#' @keywords internal
#' @noRd
checkpoint <- function(progress, kept) {
  if (!is.null(progress)) progress$kept <- kept
}
