#' Minimize a failing R script to a reproducible example
#'
#' Reduces a failing piece of R code to the smallest subset of its top-level
#' statements that still triggers the same failure. The result is a *one-minimal*
#' example: removing any remaining statement makes the failure disappear. This is
#' the form requested when reporting bugs or asking for help, and the part of
#' preparing such an example that is usually done by hand.
#'
#' By default `minex()` first runs the whole input to record the failure it
#' produces (its condition message and class), then uses [ddmin()] to search for
#' a minimal subset that reproduces it. Each candidate is evaluated in a separate
#' R process so that dependencies between statements and their side effects are
#' respected; removing a statement that a later one needs typically changes the
#' error, and such a removal is therefore rejected.
#'
#' Supply a custom `oracle` to minimize against any condition you can express as
#' a predicate, rather than against the recorded failure. The oracle receives a
#' character vector of statements and must return a single logical.
#'
#' @param file Path to a file containing the R code to minimize. Ignored if
#'   `code` is supplied.
#' @param code A character vector of R source lines, or a single string. Takes
#'   precedence over `file`.
#' @param oracle Optional predicate taking a character vector of statements and
#'   returning a single logical. When supplied, the target failure is not
#'   recorded automatically and `match` is ignored; you are fully responsible for
#'   defining what counts as reproducing the failure.
#' @param match How a candidate's failure must match the recorded one when no
#'   `oracle` is given: `"message"` (identical message, the default), `"class"`
#'   (shares a condition class) or `"both"`. Matching on the message is usually
#'   right, because an over-reduced fragment tends to fail with a different
#'   message (for example "object not found").
#' @param backend Either `"callr"` (evaluate each candidate in a fresh R process,
#'   the default and the only choice that fully isolates state) or `"inprocess"`
#'   (evaluate in the current session, faster but without isolation).
#' @param timeout Maximum seconds allowed for a single `callr` evaluation.
#' @param verbose Logical. If `TRUE`, report progress.
#'
#' @return An object of class `"minex_result"`: a list with the minimized `code`
#'   (a character vector of statements), the `original` statements, the statement
#'   counts `n_original` and `n_minimal`, the number of `oracle_calls`, the
#'   recorded `target` failure (or `NULL` for a custom oracle), and the `match`
#'   and `backend` settings.
#'
#' @seealso [ddmin()] for the underlying algorithm and [reduce_rows()] for
#'   reducing data frames.
#'
#' @examples
#' # A failing script padded with irrelevant setup.
#' script <- c(
#'   "a <- 10",
#'   "b <- 20",
#'   "log('not a number')"
#' )
#' res <- minex(code = script, backend = "inprocess")
#' res
#' cat(as.character(res), "\n")
#'
#' # A failure that genuinely depends on an earlier statement: both are kept.
#' script2 <- c(
#'   "x <- c(1, 2, NA)",
#'   "m <- mean(x)",
#'   "if (is.na(m)) stop('mean is NA')"
#' )
#' minex(code = script2, backend = "inprocess")
#'
#' \donttest{
#' # The default backend runs candidates in fresh R processes.
#' minex(code = script)
#' }
#' @export
minex <- function(file = NULL,
                  code = NULL,
                  oracle = NULL,
                  match = c("message", "class", "both"),
                  backend = c("callr", "inprocess"),
                  timeout = 60,
                  verbose = FALSE) {
  match <- match.arg(match)
  backend <- match.arg(backend)

  if (is.null(code)) {
    if (is.null(file)) {
      stop("Supply either `file` or `code`.", call. = FALSE)
    }
    if (!file.exists(file)) {
      stop("File not found: ", file, call. = FALSE)
    }
    code <- readLines(file, warn = FALSE)
  }

  statements <- split_statements(code)
  if (length(statements) < 1L) {
    stop("No parseable R statements were found in the input.", call. = FALSE)
  }

  target <- NULL
  if (is.null(oracle)) {
    target <- run_code(statements, backend = backend, timeout = timeout)
    if (!isTRUE(target$error)) {
      stop("The input ran without error; there is nothing to minimize.",
           call. = FALSE)
    }
    oracle <- make_target_oracle(
      target,
      match = match,
      backend = backend,
      timeout = timeout
    )
  } else if (!is.function(oracle)) {
    stop("`oracle` must be a function.", call. = FALSE)
  }

  calls <- 0L
  counting_oracle <- function(stmts) {
    calls <<- calls + 1L
    isTRUE(oracle(stmts))
  }

  minimal <- ddmin(statements, counting_oracle, verbose = verbose)

  structure(
    list(
      code = minimal,
      original = statements,
      n_original = length(statements),
      n_minimal = length(minimal),
      oracle_calls = calls,
      target = target,
      match = match,
      backend = backend
    ),
    class = "minex_result"
  )
}
