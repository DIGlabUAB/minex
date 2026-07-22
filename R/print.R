#' @export
print.minex_result <- function(x, ...) {
  incomplete <- !isTRUE(x$complete)
  cat(sprintf(
    "<minex_result> %d statement(s) reduced to %d (%d oracle call(s)%s)\n",
    x$n_original, x$n_minimal, x$oracle_calls,
    if (incomplete) ", incomplete" else ""
  ))
  if (!is.null(x$target) && length(x$target$message) &&
      !is.na(x$target$message)) {
    msg <- x$target$message
    if (nchar(msg) > 60L) msg <- paste0(substr(msg, 1L, 57L), "...")
    cat(sprintf("target failure: %s\n", msg))
  }
  if (incomplete) {
    # Keep "may still be reducible" on ONE line so tools/tests can grep it.
    cat("note: reproduces the failure but may still be reducible;",
        "re-run with a higher `max_oracle_calls`.\n")
  }
  if (identical(x$granularity, "expression") &&
      !is.null(x$n_chars_original) && !is.null(x$n_chars_minimal)) {
    cat(sprintf("sub-expression: %d -> %d characters\n",
                x$n_chars_original, x$n_chars_minimal))
  }
  cat(strrep("-", 48L), "\n", sep = "")
  cat(paste(x$code, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' @export
format.minex_result <- function(x, ...) {
  paste(x$code, collapse = "\n")
}

#' @export
as.character.minex_result <- function(x, ...) {
  paste(x$code, collapse = "\n")
}
