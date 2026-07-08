#' Delta debugging
#'
#' General implementation of the `ddmin` minimization algorithm of Zeller and
#' Hildebrandt (2002). Given a collection of elements and a predicate that
#' reports whether a subset still exhibits some behavior of interest, `ddmin()`
#' returns a subset that is *one-minimal*: the predicate holds for it, but fails
#' for every subset obtained by removing a single element.
#'
#' The algorithm partitions the current candidate into `n` blocks (starting with
#' `n = 2`). It first tests whether any single block reproduces the behavior; if
#' so it continues with that block. Otherwise it tests each complement (the
#' candidate with one block removed) and continues with the first that
#' reproduces. If neither succeeds the granularity is doubled, up to the point
#' where each element sits in its own block, which guarantees one-minimality.
#'
#' Results of the predicate are cached on the set of element indices, so an
#' identical configuration is never evaluated twice.
#'
#' @param items A list or atomic vector of elements to minimize.
#' @param interesting A predicate applied to a subset of `items`, in the same
#'   form as `items`, returning a single logical. It should return `TRUE` when
#'   the subset still reproduces the behavior of interest.
#' @param verbose Logical. If `TRUE`, report each reduction step via
#'   [message()].
#'
#' @return The one-minimal subset of `items`, in the original order.
#'
#' @references
#' Zeller A, Hildebrandt R (2002). "Simplifying and Isolating Failure-Inducing
#' Input." *IEEE Transactions on Software Engineering*, 28(2), 183-200.
#' \doi{10.1109/32.988498}
#'
#' @seealso [minex()] for the script-reduction front end and [reduce_rows()] for
#'   reducing data frames.
#'
#' @examples
#' # Reduce a sentence to the single word a predicate depends on.
#' words <- strsplit("the quick brown fox", " ")[[1]]
#' ddmin(words, function(s) "fox" %in% s)
#'
#' # When several elements are jointly required, all of them are kept.
#' nums <- 1:6
#' ddmin(nums, function(s) sum(s) >= 11 && 6 %in% s)
#' @export
ddmin <- function(items, interesting, verbose = FALSE) {
  if (!is.function(interesting)) {
    stop("`interesting` must be a function.", call. = FALSE)
  }
  n_items <- length(items)
  if (n_items == 0L) {
    return(items)
  }

  # Cache predicate results keyed on the sorted index set.
  cache <- new.env(parent = emptyenv())
  test <- function(idx) {
    idx <- sort(unique(idx))
    key <- paste(idx, collapse = ",")
    cached <- cache[[key]]
    if (!is.null(cached)) {
      return(cached)
    }
    result <- isTRUE(interesting(items[idx]))
    assign(key, result, envir = cache)
    result
  }

  if (!test(seq_len(n_items))) {
    stop("`interesting` is FALSE for the full set; nothing to minimize.",
         call. = FALSE)
  }

  kept <- seq_len(n_items)
  n <- 2L
  repeat {
    len <- length(kept)
    if (len < 2L) {
      break
    }
    groups <- as.integer(cut(seq_len(len), breaks = min(n, len), labels = FALSE))
    blocks <- unname(split(kept, groups))

    # 1. Can a single block reproduce the behavior on its own?
    hit <- Find(function(b) test(b), blocks)
    if (!is.null(hit)) {
      kept <- hit
      n <- 2L
      if (verbose) {
        message(sprintf("ddmin: reduced to block (%d element(s))", length(kept)))
      }
      next
    }

    # 2. Can a complement (candidate minus one block) reproduce it?
    complements <- lapply(blocks, function(b) setdiff(kept, b))
    hit <- Find(function(b) length(b) > 0L && test(b), complements)
    if (!is.null(hit)) {
      kept <- hit
      n <- max(n - 1L, 2L)
      if (verbose) {
        message(sprintf("ddmin: reduced to complement (%d element(s))",
                        length(kept)))
      }
      next
    }

    # 3. Increase granularity, or stop once blocks are singletons.
    if (n >= len) {
      break
    }
    n <- min(2L * n, len)
  }

  items[kept]
}
