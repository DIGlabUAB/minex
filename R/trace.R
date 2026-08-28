#' @keywords internal
#' @noRd
verbose_enabled <- function(verbose) !isFALSE(verbose)

#' @keywords internal
#' @noRd
wants_trace <- function(verbose) identical(verbose, "trace")

# Assemble the accumulated trace rows into a data frame (or NULL if unused).
#' @keywords internal
#' @noRd
build_trace <- function(rows) {
  if (length(rows) == 0L) {
    return(NULL)
  }
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}
