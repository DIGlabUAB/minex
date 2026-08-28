#' @keywords internal
#' @noRd
remove_removable_singles <- function(kept, test, progress = NULL) {
  repeat {
    changed <- FALSE
    i <- 1L
    while (i <= length(kept)) {
      if (length(kept) > 1L && isTRUE(test(kept[-i]))) {
        kept <- kept[-i]
        checkpoint(progress, kept)
        changed <- TRUE
        # do not advance i: the element now at i is new
      } else {
        i <- i + 1L
      }
    }
    if (!changed) {
      break
    }
  }
  kept
}
