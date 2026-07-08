#' Split R source into top-level statements
#'
#' Parses `code` and returns the source text of each top-level expression,
#' preserving the user's original formatting where source references are
#' available. Standalone comments are dropped, since they are not part of any
#' expression.
#'
#' @param code A character vector of R source lines, or a single string.
#' @return A character vector with one element per top-level statement.
#' @keywords internal
#' @noRd
split_statements <- function(code) {
  text <- paste(code, collapse = "\n")
  exprs <- parse(text = text, keep.source = TRUE)
  srcref <- attr(exprs, "srcref")

  if (is.null(srcref) || length(srcref) == 0L) {
    # Fall back to deparsing each expression if source references are absent.
    return(vapply(
      exprs,
      function(e) paste(deparse(e), collapse = "\n"),
      character(1)
    ))
  }

  vapply(
    srcref,
    function(sr) paste(as.character(sr), collapse = "\n"),
    character(1)
  )
}
