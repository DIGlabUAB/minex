#' @keywords internal
#' @noRd
remove_removable_singles <- function(kept, test) {
  repeat {
    changed <- FALSE
    i <- 1L
    while (i <= length(kept)) {
      if (length(kept) > 1L && isTRUE(test(kept[-i]))) {
        kept <- kept[-i]
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
