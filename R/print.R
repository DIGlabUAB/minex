#' @export
print.minex_result <- function(x, ...) {
  cat(sprintf(
    "<minex_result> %d statement(s) reduced to %d (%d oracle call(s))\n",
    x$n_original, x$n_minimal, x$oracle_calls
  ))
  if (!is.null(x$target) && !is.na(x$target$message)) {
    msg <- x$target$message
    if (nchar(msg) > 60L) {
      msg <- paste0(substr(msg, 1L, 57L), "...")
    }
    cat(sprintf("target failure: %s\n", msg))
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
