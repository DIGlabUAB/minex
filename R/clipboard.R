#' @keywords internal
#' @noRd
read_clipboard <- function() {
  if (!requireNamespace("clipr", quietly = TRUE)) {
    stop("Reading the clipboard needs the 'clipr' package. ",
         "Install it, or pass `code`/`file`.", call. = FALSE)
  }
  if (!clipr::clipr_available()) {
    stop("No clipboard available (headless session?). Pass `code`/`file`.",
         call. = FALSE)
  }
  clipr::read_clip()
}
