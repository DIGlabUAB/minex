#' @keywords internal
#' @noRd
pt_data <- function(text) {
  exprs <- parse(text = text, keep.source = TRUE)
  pd <- utils::getParseData(exprs)
  if (is.null(pd)) {
    stop("getParseData returned NULL (keep.source not honored).", call. = FALSE)
  }
  pd
}

#' Named vector mapping each parse-data row id to its parent id.
#' @keywords internal
#' @noRd
pt_parent_map <- function(pd) {
  stats::setNames(pd$parent, pd$id)
}

#' Depth of each parse-data row (root tokens = 0), by walking `parent` ids.
#' @keywords internal
#' @noRd
pt_depth <- function(pd) {
  id_to_parent <- pt_parent_map(pd)
  vapply(pd$id, function(id) {
    depth <- 0L
    p <- id_to_parent[[as.character(id)]]
    while (!is.null(p) && !is.na(p) && p > 0L) {
      depth <- depth + 1L
      p <- id_to_parent[[as.character(p)]]
    }
    depth
  }, integer(1))
}

#' @keywords internal
#' @noRd
pt_is_pipe_row <- function(pd) {
  pd$token == "PIPE" | (pd$token == "SPECIAL" & pd$text == "%>%")
}

# Convert (line,col) offsets from getParseData into 1-based character indices
# into `text`. Requires the line-start offsets of `text`.
#' @keywords internal
#' @noRd
pt_line_offsets <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0L) lines <- ""
  # character index at which each line starts (1-based), accounting for \n
  cumsum(c(1L, utils::head(nchar(lines) + 1L, -1L)))
}

#' @keywords internal
#' @noRd
pt_char_index <- function(line, col, line_starts) {
  line_starts[line] + (col - 1L)
}

# Walk the left-nested pipe spine of every top-level pipe chain.
#' @keywords internal
#' @noRd
pt_pipe_stages <- function(text) {
  pd <- pt_data(text)
  starts <- pt_line_offsets(text)
  pipe_rows <- which(pt_is_pipe_row(pd))
  if (length(pipe_rows) == 0L) return(list())

  # Group pipe operators by their top-most chain: an operator belongs to the
  # chain rooted at the highest ancestor expr that is itself a pipe expr.
  # For each chain, produce ordered stage spans. Implementation walks each
  # pipe operator and its right-hand operand; the leftmost operand of the
  # whole chain is the data source.
  # (Detailed spine-walk: for each PIPE row, the RHS stage span runs from just
  # after the operator to the end of the operator's parent expr's RHS child.)
  chains <- list()
  # Group operators sharing a spine by climbing parents until the parent is not
  # a pipe expr; use that ancestor id as the chain key.
  parent_of <- pt_parent_map(pd)
  is_pipe_expr <- function(expr_id) {
    any(pt_is_pipe_row(pd) & pd$parent == expr_id)
  }
  chain_key <- function(op_row) {
    id <- pd$parent[op_row]
    repeat {
      p <- parent_of[[as.character(id)]]
      if (is.null(p) || is.na(p) || p <= 0L || !is_pipe_expr(p)) break
      id <- p
    }
    id
  }
  keys <- vapply(pipe_rows, chain_key, integer(1))
  for (k in unique(keys)) {
    ops <- pipe_rows[keys == k]
    ops <- ops[order(pd$col1[ops])]               # left-to-right
    # data source = everything left of the first operator within chain expr k
    chain_from <- pt_char_index(pd$line1[pd$id == k], pd$col1[pd$id == k], starts)
    first_op_from <- pt_char_index(pd$line1[ops[1]], pd$col1[ops[1]], starts)
    stages <- list(list(
      from = chain_from, to = first_op_from - 1L, reducible = FALSE))  # data source
    for (i in seq_along(ops)) {
      op <- ops[i]
      op_from <- pt_char_index(pd$line1[op], pd$col1[op], starts)
      # stage RHS ends at the start of the next operator (or chain end)
      to <- if (i < length(ops)) {
        pt_char_index(pd$line1[ops[i + 1]], pd$col1[ops[i + 1]], starts) - 1L
      } else {
        pt_char_index(pd$line2[pd$id == k], pd$col2[pd$id == k], starts)
      }
      # reducible span = operator + its RHS stage (delete both together)
      stages[[length(stages) + 1L]] <- list(from = op_from, to = to, reducible = TRUE)
    }
    chains[[length(chains) + 1L]] <- stages
  }
  chains
}

#' @keywords internal
#' @noRd
pt_call_args <- function(text) {
  pd <- pt_data(text)
  starts <- pt_line_offsets(text)
  # A call expr has a child that is a function head followed by '(' ... ')'.
  # Identify calls by the presence of a "'('" token whose parent expr also
  # contains a SYMBOL_FUNCTION_CALL / the callee.
  open_parens <- pd$id[pd$token == "'('"]
  out <- list()
  for (op in open_parens) {
    call_expr <- pd$parent[pd$id == op]
    kids <- pd[pd$parent == call_expr, , drop = FALSE]
    kids <- kids[order(kids$col1 + 1000L * kids$line1), ]
    paren <- pd[pd$id == op, ]
    paren_key <- paren$col1 + 1000L * paren$line1
    # A '(' opens a genuine function call only when its parent expr has a
    # callee: a sibling `expr` token positioned strictly before the '('.
    # Grouping parens `(a + b)` have '(' as their very first child (no expr
    # precedes it); control-flow parens (`if (...)`, `for (...)`, `while
    # (...)`) are preceded by a keyword token (IF/FOR/WHILE), not an expr.
    # Verified empirically against getParseData for f(...), (a + b),
    # if (...) foo(...), for (...) g(...), pkg::fn(...), and x$m(...).
    has_callee <- any(kids$token == "expr" &
                         (kids$col1 + 1000L * kids$line1) < paren_key)
    if (!has_callee) next
    spans <- list()
    # order all relevant tokens (args, commas, named marks) by position
    seq_tokens <- kids[kids$token %in% c("expr", "','", "SYMBOL_SUB", "EQ_SUB"), , drop = FALSE]
    seq_tokens <- seq_tokens[order(seq_tokens$col1 + 1000L * seq_tokens$line1), ]
    # Drop the callee (function-head expr): it is a child `expr` of the call that
    # sits BEFORE the '(' and is NOT an argument. Keep only tokens after the open
    # paren, otherwise the function name is treated as a positional arg (deleting
    # it yields "(a, b, c)", which does not parse).
    seq_tokens <- seq_tokens[(seq_tokens$col1 + 1000L * seq_tokens$line1) > paren_key, , drop = FALSE]
    for (ri in which(seq_tokens$token == "expr")) {
      # positional iff the token immediately before is not a named-arg marker (=)
      prev_named <- ri > 1L && seq_tokens$token[ri - 1L] == "EQ_SUB"
      if (prev_named) next   # value of a named arg: skip (deferred)
      node <- seq_tokens[ri, ]
      node_from <- pt_char_index(node$line1, node$col1, starts)
      node_to   <- pt_char_index(node$line2, node$col2, starts)
      # comma selection by ABSOLUTE position:
      prev_comma <- if (ri > 1L && seq_tokens$token[ri - 1L] == "','") seq_tokens[ri - 1L, ] else NULL
      next_comma <- if (ri < nrow(seq_tokens) && seq_tokens$token[ri + 1L] == "','") seq_tokens[ri + 1L, ] else NULL
      if (!is.null(prev_comma)) {
        from <- pt_char_index(prev_comma$line1, prev_comma$col1, starts); to <- node_to
      } else if (!is.null(next_comma)) {
        from <- node_from; to <- pt_char_index(next_comma$line2, next_comma$col2, starts)
      } else {
        from <- node_from; to <- node_to
      }
      spans[[length(spans) + 1L]] <- list(from = from, to = to)
    }
    if (length(spans)) out[[length(out) + 1L]] <- spans
  }
  out
}

#' @keywords internal
#' @noRd
pt_delete <- function(text, spans) {
  # sort spans by `from` descending so deletions don't shift later offsets
  spans <- spans[order(vapply(spans, `[[`, numeric(1), "from"), decreasing = TRUE)]
  out <- text
  for (s in spans) {
    left <- substr(out, 1L, s$from - 1L)
    right <- substr(out, s$to + 1L, nchar(out))
    # Deleting [from, to] can create exactly one new adjacency: the character
    # that used to precede `from` now sits next to the character that used to
    # follow `to`. If that seam puts a space/tab against a space/tab, drop one
    # of the two seam characters -- and ONLY that one. This must never touch
    # whitespace elsewhere in the string (e.g. inside surviving string
    # literals), so no global regex may run over `out`.
    if (grepl("[ \t]$", left) && grepl("^[ \t]", right)) {
      right <- sub("^[ \t]", "", right)
    }
    out <- paste0(left, right)
  }
  out
}

#' @keywords internal
#' @noRd
pt_parses <- function(text) {
  !inherits(try(parse(text = text), silent = TRUE), "try-error")
}

#' @keywords internal
#' @noRd
pt_has_trailing_comment <- function(pd, line) {
  any(pd$token == "COMMENT" & pd$line1 == line)
}
