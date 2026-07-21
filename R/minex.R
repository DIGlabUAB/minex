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
#' @param file Path to a file containing the R code to minimize. Used only when
#'   neither `code` nor `clipboard` is supplied.
#' @param code A character vector of R source lines, or a single string. Takes
#'   precedence over both `clipboard` and `file`.
#' @param clipboard Logical. If `TRUE`, read the R code to minimize from the
#'   system clipboard. Takes precedence over `file`, but is overridden by `code`.
#'   Input precedence is `code > clipboard > file`; supplying more than one
#'   source emits a warning and the highest-precedence source is used.
#' @param oracle Optional predicate taking a character vector of statements and
#'   returning a single logical. When supplied, the target failure is not
#'   recorded automatically and `condition`, `match` and failure-point truncation
#'   are all bypassed; you are fully responsible for defining what counts as
#'   reproducing the failure.
#' @param condition Which kind of condition to target: `"error"` (the default),
#'   `"warning"`, `"message"`, or `"any"` (the most severe condition present,
#'   error > warning > message). Ignored when a custom `oracle` is supplied.
#' @param match How a candidate's failure must match the recorded one when no
#'   `oracle` is given: `"message"` (identical message, the default), `"class"`
#'   (shares a condition class) or `"both"`. Matching on the message is usually
#'   right, because an over-reduced fragment tends to fail with a different
#'   message (for example "object not found").
#' @param algorithm The reduction strategy passed to [ddmin()]: `"cdd"`
#'   (convergent delta debugging, the default) or `"ddmin"` (the classic
#'   block-halving loop).
#' @param backend Either `"callr"` (evaluate each candidate in a fresh R process,
#'   the default and the only choice that fully isolates state) or `"inprocess"`
#'   (evaluate in the current session, faster but without isolation; a script's
#'   side effects other than options are not sandboxed).
#' @param timeout Maximum seconds allowed for a single `callr` evaluation.
#' @param max_oracle_calls Numeric upper bound on the number of oracle
#'   evaluations, passed to [ddmin()]. When the budget is exhausted the reduction
#'   stops early with a warning; the result still reproduces the failure but may
#'   not be one-minimal.
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
                  clipboard = FALSE,
                  oracle = NULL,
                  condition = c("error", "warning", "message", "any"),
                  match = c("message", "class", "both"),
                  algorithm = c("cdd", "ddmin"),
                  backend = c("callr", "inprocess"),
                  timeout = 60,
                  max_oracle_calls = Inf,
                  verbose = FALSE) {
  condition <- match.arg(condition)
  match     <- match.arg(match)
  algorithm <- match.arg(algorithm)
  backend   <- match.arg(backend)

  # Input precedence: code > clipboard > file; warn on multi-source.
  sources <- c(code = !is.null(code), clipboard = isTRUE(clipboard),
               file = !is.null(file))
  if (sum(sources) > 1L) {
    warning("Multiple input sources supplied; using ",
            names(sources)[which(sources)[1]], " (code > clipboard > file).",
            call. = FALSE)
  }
  if (!is.null(code)) {
    # use code as-is
  } else if (isTRUE(clipboard)) {
    code <- read_clipboard()
  } else if (!is.null(file)) {
    if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
    code <- readLines(file, warn = FALSE)
  } else {
    stop("Supply `code`, `file`, or `clipboard = TRUE`.", call. = FALSE)
  }

  statements <- split_statements(code)
  if (length(statements) < 1L) {
    stop("No parseable R statements were found in the input.", call. = FALSE)
  }
  statements_full <- statements   # pre-truncation, for honest `original`/`n_original`

  target <- NULL
  if (is.null(oracle)) {
    matcher <- build_matcher(match)
    full <- run_code(statements, backend = backend, timeout = timeout)
    # Choose the target condition, then its defining statement index.
    target_cond <- pick_target_condition(full$conditions, condition)
    if (is.null(target_cond)) {
      stop(sprintf(
        "The input produced no %s; nothing to minimize.", condition),
        call. = FALSE)
    }
    idx <- pick_target_index(full$conditions, target_cond, matcher, condition)
    target <- list(message = target_cond$message, classes = target_cond$classes,
                   conditions = full$conditions, failing_index = idx)
    oracle <- function(stmts) {
      result <- run_code(stmts, backend = backend, timeout = timeout)
      cand <- pick_target_condition(result$conditions, condition)
      if (is.null(cand)) return(FALSE)
      isTRUE(matcher(cand, target_cond))
    }
    # Free failure-point truncation, with a fallback for flaky scripts: only
    # keep the truncated set if it still reproduces (spec section 2). One oracle call.
    if (!is.na(idx)) {
      truncated <- truncate_statements(statements, idx)
      if (length(truncated) < length(statements) && isTRUE(oracle(truncated))) {
        statements <- truncated
      }
    }
  } else if (!is.function(oracle)) {
    stop("`oracle` must be a function.", call. = FALSE)
  }

  info <- new.env()
  minimal <- ddmin(statements, function(s) isTRUE(oracle(s)),
                   algorithm = algorithm, max_oracle_calls = max_oracle_calls,
                   verbose = verbose, .info = info)

  if (!isTRUE(info$complete)) {
    warning("minex stopped early; result reproduces the failure but may still ",
            "be reducible. Re-run with a higher `max_oracle_calls`.",
            call. = FALSE)
  }

  structure(
    list(code = minimal, original = statements_full,
         n_original = length(statements_full), n_minimal = length(minimal),
         oracle_calls = info$oracle_calls, target = target, match = match,
         backend = backend, complete = info$complete, algorithm = algorithm,
         condition = condition, max_oracle_calls = max_oracle_calls,
         trace = info$trace),
    class = "minex_result"
  )
}
