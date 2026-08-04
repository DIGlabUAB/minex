#' @export
print.minex_result <- function(x, ...) {
  incomplete <- !isTRUE(x$complete)
  .stmt_moved <- x$n_minimal < x$n_original
  .char_moved <- !is.null(x$n_chars_original) && !is.null(x$n_chars_minimal) &&
    x$n_chars_minimal < x$n_chars_original
  # Lead with whichever axis moved. Where a script is one function plus a
  # call, the statement count cannot change, so leading with it reports a
  # successful reduction as a failure.
  if (.stmt_moved || !.char_moved) {
    cat(sprintf(
      "<minex_result> %d statement(s) reduced to %d (%d oracle call(s)%s)\n",
      x$n_original, x$n_minimal, x$oracle_calls,
      if (incomplete) ", incomplete" else ""
    ))
  } else {
    cat(sprintf(
      "<minex_result> %d -> %d characters, %d statement(s) unchanged (%d oracle call(s)%s)\n",
      x$n_chars_original, x$n_chars_minimal, x$n_original, x$oracle_calls,
      if (incomplete) ", incomplete" else ""
    ))
  }
  if (!.stmt_moved && !.char_moved && identical(x$granularity, "statement")) {
    cat("note: nothing removable at statement granularity;",
        "try granularity = \"expression\".\n")
  }
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
  if (identical(x$granularity, "expression") && (.stmt_moved || !.char_moved) &&
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
