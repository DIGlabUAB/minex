#' Run a code fragment and capture whether it errors
#'
#' @param code Character vector of R statements.
#' @param backend Either `"callr"` (evaluate in a fresh R process, the default)
#'   or `"inprocess"` (evaluate in a new environment in the current session).
#' @param timeout Maximum seconds to allow a `callr` evaluation to run.
#' @return A list with elements `error` (logical), `message` (character or `NA`)
#'   and `classes` (the condition's class vector, excluding `"condition"`).
#' @keywords internal
#' @noRd
run_code <- function(code, backend = c("callr", "inprocess"), timeout = 60) {
  backend <- match.arg(backend)
  src <- paste(code, collapse = "\n")

  runner <- function(src) {
    tryCatch(
      {
        eval(parse(text = src), envir = new.env(parent = globalenv()))
        list(error = FALSE, message = NA_character_, classes = character(0))
      },
      error = function(e) {
        list(
          error = TRUE,
          message = conditionMessage(e),
          classes = setdiff(class(e), "condition")
        )
      }
    )
  }

  if (backend == "inprocess") {
    return(runner(src))
  }

  # In a separate process a timeout or a hard crash means we could not observe
  # the target error, so the fragment is treated as not reproducing it.
  tryCatch(
    callr::r(runner, args = list(src = src), timeout = timeout),
    error = function(e) {
      list(error = FALSE, message = NA_character_, classes = character(0))
    }
  )
}

#' Build an oracle that matches a captured target failure
#'
#' @param target A captured failure, as returned by [run_code()].
#' @param match How a candidate's failure must match the target: `"message"`
#'   (identical condition message, the default), `"class"` (shares at least one
#'   condition class) or `"both"`.
#' @inheritParams run_code
#' @return A predicate suitable for [ddmin()]: it takes a character vector of
#'   statements and returns `TRUE` when they reproduce the target failure.
#' @keywords internal
#' @noRd
make_target_oracle <- function(target,
                               match = c("message", "class", "both"),
                               backend = "callr",
                               timeout = 60) {
  match <- match.arg(match)
  force(target)

  function(code) {
    result <- run_code(code, backend = backend, timeout = timeout)
    if (!isTRUE(result$error)) {
      return(FALSE)
    }
    message_ok <- identical(result$message, target$message)
    class_ok <- length(intersect(result$classes, target$classes)) > 0L
    switch(match,
      message = message_ok,
      class = class_ok,
      both = message_ok && class_ok
    )
  }
}
