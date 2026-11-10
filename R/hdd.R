# This specialized HDD*-style pass is based on Misherghi and Su (ICSE 2006),
# DOI: 10.1145/1134285.1134307. It reduces only the R constructs for which this
# package defines safe deletion spans, rather than every node in a general tree.
# Collect reducible deletion spans grouped by tree depth (shallow -> deep),
# excluding spans on lines carrying a trailing comment.
#' @keywords internal
#' @noRd
hdd_reducible_by_depth <- function(text) {
  pd <- pt_data(text)
  depth <- pt_depth(pd)
  spans <- list()   # each: list(from, to, depth)

  for (chain in pt_pipe_stages(text)) {
    for (stage in chain) {
      if (isTRUE(stage$reducible)) {
        spans[[length(spans) + 1L]] <- list(from = stage$from, to = stage$to,
                                             depth = 0L)  # spine depth handled below
      }
    }
  }
  for (call in pt_call_args(text)) {
    for (s in call) {
      spans[[length(spans) + 1L]] <- list(from = s$from, to = s$to, depth = 0L)
    }
  }
  if (length(spans) == 0L) return(list())

  # Assign each span a depth = pt_depth of the smallest expr node fully containing it.
  starts <- pt_line_offsets(text)
  node_from <- pt_char_index(pd$line1, pd$col1, starts)
  node_to   <- pt_char_index(pd$line2, pd$col2, starts)
  span_depth <- function(s) {
    covering <- which(pd$token == "expr" & node_from <= s$from & node_to >= s$to)
    if (length(covering) == 0L) return(0L)
    max(depth[covering])
  }
  for (i in seq_along(spans)) spans[[i]]$depth <- span_depth(spans[[i]])

  # drop spans that traverse any line with a trailing comment (conservative):
  # a multi-line span (e.g. a pipe stage) can start on a clean line but cross
  # a later line that carries a trailing comment, so every line from the
  # span's start through its end must be checked, not just the start line.
  keep <- vapply(spans, function(s) {
    fl <- which(starts <= s$from); fl <- fl[length(fl)]
    tl <- which(starts <= s$to);   tl <- tl[length(tl)]
    !any(vapply(fl:tl, function(l) pt_has_trailing_comment(pd, l), logical(1)))
  }, logical(1))
  spans <- spans[keep]
  if (length(spans) == 0L) return(list())

  # group by depth, shallow -> deep
  by <- split(spans, vapply(spans, `[[`, integer(1), "depth"))
  by[order(as.integer(names(by)))]
}

#' @keywords internal
#' @noRd
hdd_star <- function(text, reproduces, max_oracle_calls = Inf, info = new.env(),
                     verbose = FALSE) {
  validate_oracle_budget(max_oracle_calls)
  context <- oracle_context(reproduces)
  if (is.null(context)) {
    context <- new_oracle_context(max_oracle_calls, verbose)
    reproduces <- oracle_wrap(reproduces, context)
  } else if (identical(context$owner, "hdd_memo")) {
    context$max_calls <- context$budget_calls + max_oracle_calls
    context$verbose <- verbose
  }
  calls_before <- context$calls
  trace_before <- length(context$rows)
  current <- text
  complete <- TRUE
  repeat {                                   # outer fixpoint => HDD*
    levels <- hdd_reducible_by_depth(current)
    if (length(levels) == 0L) break
    progressed <- FALSE
    for (level in levels) {                  # shallow -> deep
      before <- current
      sub <- new.env()
      # ddmin over this level's spans: item = a span; interesting = candidate
      # with the KEPT spans re-inserted (i.e. delete the NON-kept spans) reproduces.
      idx <- seq_along(level)
      context$level <- level[[1]]$depth
      interesting <- oracle_inherit(function(keep_idx) {
        drop <- setdiff(idx, keep_idx)
        cand <- pt_delete(current, level[drop])
        if (!pt_parses(cand)) {
          oracle_trace(context, FALSE, event = "parse_rejected")
          return(FALSE)
        }
        isTRUE(reproduces(cand))
      }, context)
      # We want a one-minimal set of spans to KEEP such that dropping the rest
      # still reproduces; ddmin finds that keep-set, then we delete the rest.
      kept <- ddmin(idx, interesting, verbose = verbose, .info = sub)
      if (!isTRUE(sub$complete)) complete <- FALSE
      # ddmin NEVER returns an empty keep-set (sweep.R guards `length(kept) > 1`
      # and cdd_reduce rejects empty complements). So a depth level whose spans
      # are ALL unnecessary would keep one spurious span, violating the spec's
      # 1-tree-minimality guarantee (R-verified: `f(aa, y = 2, cc)` reproducing
      # on `y = 2` returned `f( y = 2, cc)`; `x |> rev() |> sqrt()` reproducing on
      # `x` returned itself). By one-minimality this can only happen from a
      # size-1 keep-set (a size >= 2 keep-set has every element individually
      # necessary), so probe dropping that last span too -- one counted,
      # budget-gated oracle call. When the culprit IS that span, the probe
      # candidate does not reproduce and the span is kept (no over-deletion).
      if (length(kept) == 1L && isTRUE(sub$complete)) {
        context$phase <- "verify_empty"
        context$size <- 0L
        whole <- pt_delete(current, level)
        if (pt_parses(whole)) {
          tryCatch({
            if (isTRUE(reproduces(whole))) kept <- integer(0)
          }, minex_oracle_limit_reached = function(e) complete <<- FALSE)
        } else oracle_trace(context, FALSE, event = "parse_rejected")
      }
      current <- pt_delete(current, level[setdiff(idx, kept)])
      if (!complete) break
      if (!identical(current, before)) { progressed <- TRUE; break }  # reparse
    }
    if (!complete || !progressed) break
  }
  info$oracle_calls <- context$calls - calls_before
  info$complete <- complete
  info$trace <- if (length(context$rows) > trace_before) {
    build_trace(context$rows[seq.int(trace_before + 1L, length(context$rows))])
  } else NULL
  current
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
#' @noRd
hdd_make_oracle <- function(run_reproduces) {
  context <- new_oracle_context()
  context$owner <- "hdd_memo"
  oracle_wrap(run_reproduces, context)
}
