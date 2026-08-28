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
  current <- text
  total_calls <- 0L
  complete <- TRUE
  do_trace <- wants_trace(verbose)
  trace_parts <- list()
  repeat {                                   # outer fixpoint => HDD*
    levels <- hdd_reducible_by_depth(current)
    if (length(levels) == 0L) break
    progressed <- FALSE
    for (level in levels) {                  # shallow -> deep
      before <- current
      sub <- new.env()
      remaining <- if (is.finite(max_oracle_calls)) max_oracle_calls - total_calls else Inf
      if (is.finite(remaining) && remaining < 1) { complete <- FALSE; break }
      # ddmin over this level's spans: item = a span; interesting = candidate
      # with the KEPT spans re-inserted (i.e. delete the NON-kept spans) reproduces.
      idx <- seq_along(level)
      interesting <- function(keep_idx) {
        drop <- setdiff(idx, keep_idx)
        cand <- pt_delete(current, level[drop])
        if (!pt_parses(cand)) return(FALSE)   # free reparse reject (no budget)
        isTRUE(reproduces(cand))
      }
      # We want the SMALLEST set of spans to KEEP such that dropping the rest
      # still reproduces -> ddmin finds a 1-minimal keep-set; then delete the rest.
      kept <- ddmin(idx, function(k) interesting(k),
                    max_oracle_calls = remaining, verbose = verbose, .info = sub)
      lvl_calls <- sub$oracle_calls %||% 0L
      if (!isTRUE(sub$complete)) complete <- FALSE
      if (do_trace && !is.null(sub$trace)) {
        tagged <- sub$trace
        tagged$level <- level[[1]]$depth
        trace_parts[[length(trace_parts) + 1L]] <- tagged
      }
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
        rem2 <- if (is.finite(max_oracle_calls)) max_oracle_calls - total_calls - lvl_calls else Inf
        if (!is.finite(rem2) || rem2 >= 1) {
          whole <- pt_delete(current, level)
          if (pt_parses(whole)) {
            lvl_calls <- lvl_calls + 1L
            if (isTRUE(reproduces(whole))) kept <- integer(0)
          }
        }
      }
      total_calls <- total_calls + lvl_calls
      current <- pt_delete(current, level[setdiff(idx, kept)])
      if (!identical(current, before)) { progressed <- TRUE; break }  # reparse
    }
    if (!progressed) break
  }
  info$oracle_calls <- total_calls
  info$complete <- complete
  if (do_trace) {
    info$trace <- if (length(trace_parts) == 0L) NULL else do.call(rbind, trace_parts)
  }
  current
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
#' @noRd
hdd_make_oracle <- function(run_reproduces) {
  memo <- new.env(parent = emptyenv())
  function(text) {
    key <- text
    hit <- memo[[key]]
    if (!is.null(hit)) return(hit)
    val <- isTRUE(run_reproduces(text))
    assign(key, val, envir = memo)
    val
  }
}
