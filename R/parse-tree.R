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

#' Depth of each parse-data row (root tokens = 0), by walking `parent` ids.
#' @keywords internal
#' @noRd
pt_depth <- function(pd) {
  id_to_parent <- stats::setNames(pd$parent, pd$id)
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
