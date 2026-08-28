#' Delta debugging
#'
#' General implementation of the `ddmin` minimization algorithm of Zeller and
#' Hildebrandt (2002). Given a collection of elements and a predicate that
#' reports whether a subset still exhibits some behavior of interest, `ddmin()`
#' returns a subset that is *one-minimal*: the predicate holds for it, but fails
#' for every subset obtained by removing a single element.
#'
#' The algorithm partitions the current candidate into `n` blocks (starting with
#' `n = 2`). It first tests whether any single block reproduces the behavior; if
#' so it continues with that block. Otherwise it tests each complement (the
#' candidate with one block removed) and continues with the first that
#' reproduces. If neither succeeds the granularity is doubled, up to the point
#' where each element sits in its own block, which guarantees one-minimality.
#'
#' Results of the predicate are cached on the set of element indices, so an
#' identical configuration is never evaluated twice.
#'
#' @param items A list or atomic vector of elements to minimize.
#' @param interesting A predicate applied to a subset of `items`, in the same
#'   form as `items`, returning a single logical. It should return `TRUE` when
#'   the subset still reproduces the behavior of interest.
#' @param algorithm Character. The reduction strategy for the first phase, one
#'   of `"cdd"` (convergent delta debugging, the default) or `"ddmin"` (the
#'   classic block-halving loop). Both are followed by a verification sweep that
#'   removes any remaining removable single elements.
#' @param max_oracle_calls Numeric. An upper bound on the number of predicate
#'   evaluations. Must be at least 1. When the budget is exhausted the reduction
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
#' of Probabilistic Delta Debugging." ICSE 2025. arXiv:2408.04735.
#' \url{https://arxiv.org/abs/2408.04735}
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
  if (max_oracle_calls < 1) {
    stop("`max_oracle_calls` must be at least 1.", call. = FALSE)
  }
  n_items <- length(items)
  if (n_items == 0L) {
    if (is.environment(.info)) {
      .info$oracle_calls <- 0L; .info$complete <- TRUE; .info$trace <- NULL
    }
    return(items)
  }

  cache <- new.env(parent = emptyenv())
  calls <- 0L
  complete <- TRUE
  trace_rows <- list()
  phase <- "reduce"       # updated to "verify" before the sweep
  do_trace <- wants_trace(verbose)
  say <- function(...) if (verbose_enabled(verbose)) message("minex: ", ...)

  # `exempt = TRUE` skips only the budget-LIMIT CHECK for the mandatory
  # precondition test; the call still increments `calls`. So with
  # max_oracle_calls = 1 the precondition runs (calls -> 1) and the first
  # reduce-phase call then trips the limit -> complete = FALSE. This is
  # intended: a budget under the minimum needed yields an honest incomplete.
  test <- function(idx, exempt = FALSE) {
    idx <- sort(unique(idx))
    key <- paste(idx, collapse = ",")
    cached <- cache[[key]]
    is_cached <- !is.null(cached)
    if (!is_cached && !exempt && calls >= max_oracle_calls) {
      if (do_trace) {
        trace_rows[[length(trace_rows) + 1L]] <<- list(
          call = calls, phase = phase, size = length(idx),
          kept = NA, cached = FALSE, event = "budget_exhausted")
      }
      stop(structure(
        class = c("minex_oracle_limit_reached", "error", "condition"),
        list(message = "oracle call budget exhausted", call = NULL)
      ))
    }
    result <- if (is_cached) cached else isTRUE(interesting(items[idx]))
    if (!is_cached) {
      calls <<- calls + 1L
      assign(key, result, envir = cache)
    }
    if (do_trace) {
      trace_rows[[length(trace_rows) + 1L]] <<- list(
        call = calls, phase = phase, size = length(idx),
        kept = result, cached = is_cached, event = "")
    }
    result
  }

  if (!test(seq_len(n_items), exempt = TRUE)) {
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
    .info$oracle_calls <- calls
    .info$complete <- complete
    .info$trace <- build_trace(trace_rows)
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
