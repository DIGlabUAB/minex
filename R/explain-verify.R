# Source-text heuristic: defense-in-depth, NOT a security boundary. A non-
# exhaustive list of side-effecting calls; evadable via aliasing / do.call /
# eval(parse()). Reduces accidental and obvious-malicious I/O in auto-run fixes.
.minex_denylist <- c(
  "system", "system2", "shell", "pipe", "download.file", "url",
  "socketConnection", "unlink", "file.remove", "writeLines", "write.csv",
  "write.table", "saveRDS", "readRDS", "save", "load", "source", "sink",
  "install.packages", "Sys.setenv", "Sys.getenv"
)

#' @keywords internal
#' @noRd
is_denylisted <- function(code) {
  text <- paste(code, collapse = "\n")
  patterns <- paste0("\\b", gsub(".", "\\.", .minex_denylist, fixed = TRUE), "\\s*\\(")
  any(vapply(patterns, function(p) grepl(p, text), logical(1)))
}