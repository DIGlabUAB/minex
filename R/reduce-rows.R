#' Reduce a data frame to the rows that reproduce a failure
#'
#' Often a bug only shows up with a large data frame, even though a handful of
#' rows is enough to trigger it. `reduce_rows()` applies [ddmin()] over the rows
#' of `data` and returns the smallest subset for which `predicate` still holds,
#' preserving the original row order. The result is typically small enough to
#' paste into a bug report with [dput()].
#'
#' @param data A data frame.
#' @param predicate A function taking a data frame (a subset of `data`'s rows)
#'   and returning a single logical: `TRUE` when the subset still reproduces the
#'   failure of interest.
#' @param algorithm Character. The reduction strategy for the first phase, one
#'   of `"cdd"` (default) or `"ddmin"`. Passed through to [ddmin()].
#' @param max_oracle_calls Numeric. An upper bound on the number of predicate
#'   calls. Passed through to [ddmin()].
#' @param verbose Logical. If `TRUE`, report progress.
#'
#' @return A data frame containing the one-minimal subset of rows.
#'
#' @seealso [ddmin()], [minex()].
#'
#' @examples
#' df <- data.frame(id = 1:6, value = c(3, 8, 999, 2, 5, 7))
#' # The failure: any value greater than 100.
#' reduce_rows(df, function(d) any(d$value > 100))
#' @export
reduce_rows <- function(data, predicate,
                        algorithm = c("cdd", "ddmin"),
                        max_oracle_calls = Inf,
                        verbose = FALSE) {
  algorithm <- match.arg(algorithm)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.function(predicate)) {
    stop("`predicate` must be a function.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    return(data)
  }
  if (!isTRUE(predicate(data))) {
    stop("`predicate` is FALSE for the full data; nothing to minimize.",
         call. = FALSE)
  }

  keep <- ddmin(
    seq_len(nrow(data)),
    function(rows) isTRUE(predicate(data[sort(rows), , drop = FALSE])),
    algorithm = algorithm, max_oracle_calls = max_oracle_calls, verbose = verbose
  )

  data[sort(keep), , drop = FALSE]
}
