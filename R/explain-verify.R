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

#' @keywords internal
#' @noRd
target_type <- function(target, condition) {
  if (!identical(condition, "any")) {
    return(condition)
  }
  tc <- pick_target_condition(target$conditions, "any")
  if (is.null(tc)) condition else tc$type
}

#' @keywords internal
#' @noRd
verify_fix_code <- function(fix_code, target, condition, match,
                            backend = "callr", timeout = 60) {
  na <- function(msg) list(verified = NA, verification = msg)

  if (is_denylisted(fix_code)) {
    return(na(paste0("not auto-run: contains a potentially side-effecting call; ",
                     "run manually to verify")))
  }
  stmts <- tryCatch(split_statements(fix_code), error = function(e) e)
  if (inherits(stmts, "error")) {
    return(na(paste0("fix did not parse: ", conditionMessage(stmts))))
  }
  if (length(stmts) < 1L) {
    return(na("fix contained no runnable code"))
  }
  if (is.null(target)) {
    return(na(paste0("original failure was defined by a custom oracle ",
                     "(not captured); cannot verify")))
  }

  res <- run_code(stmts, backend = backend, timeout = timeout)
  if (!isTRUE(res$observed)) {
    return(na("fix run could not be observed (timeout or crash)"))
  }

  ttype <- target_type(target, condition)
  types <- vapply(res$conditions, `[[`, character(1), "type")
  clean <- !("error" %in% types) && !(ttype %in% types)
  if (clean) {
    return(list(verified = TRUE, verification = "clean run: the target failure is gone"))
  }

  matcher <- build_matcher(match)
  cand <- pick_target_condition(res$conditions, condition)
  same <- !is.null(cand) && isTRUE(matcher(cand, target))
  list(verified = FALSE,
       verification = if (same) "same failure still present"
                      else "different failure than the original")
}
