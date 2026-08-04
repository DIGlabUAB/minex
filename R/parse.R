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
  exprs <- tryCatch(
    parse(text = text, keep.source = TRUE),
    error = function(e) {
      # "Does not parse" is a documented boundary of the tool rather than
      # an internal failure, so it is raised as a catchable classed
      # condition. Callers that feed minex generated code -- where a large
      # share of inputs do not parse -- can branch on the class.
      stop(errorCondition(
        message = paste0(
          "minex requires a parseable script, but parsing failed:\n  ",
          conditionMessage(e), "\n",
          "minex localises runtime failures by bisection; a syntax error ",
          "has no runtime\n  failure to localise. Fix the syntax error first."
        ),
        parse_error = conditionMessage(e),
        class = c("minex_parse_error", "minex_error")
      ))
    }
  )
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
