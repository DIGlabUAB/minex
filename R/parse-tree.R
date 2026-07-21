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
  cumsum(c(1L, head(nchar(lines) + 1L, -1L)))
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
