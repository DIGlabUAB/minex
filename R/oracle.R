#' @keywords internal
#' @noRd
run_code <- function(code, backend = c("callr", "inprocess"), timeout = 60) {
  backend <- match.arg(backend)
  statements <- code

  runner <- function(statements) {
    env <- new.env(parent = globalenv())
    conditions <- list()
    promoted_msgs <- character(0) # messages already recorded via warn=2 promotion
    current <- 0L

    record <- function(cond, type) {
      conditions[[length(conditions) + 1L]] <<-
        list(
          type       = type,
          message    = sub("\n$", "", conditionMessage(cond)),
          classes    = setdiff(class(cond), "condition"),
          stmt_index = as.integer(current)
        )
    }

    withCallingHandlers(
      tryCatch(
        for (i in seq_along(statements)) {
          current <- i
          eval(parse(text = statements[[i]])[[1]], envir = env)
        },
        error = function(e) {
          # Under warn>=2 a warning is promoted to an error whose message is the
          # warning text with a "(converted from warning) " prefix; the warning
          # handler already recorded it, so skip that duplicate (a class-stripped
          # re-entry). Match on the message rather than the statement index: a
          # single top-level statement (e.g. a `{...}` block) may recover the
          # promotion via its own tryCatch and then raise a genuinely different
          # error, which shares the index but not the message and must be kept.
          em <- conditionMessage(e)
          is_promotion_dup <- any(vapply(
            promoted_msgs, function(m) nzchar(m) && endsWith(em, m), logical(1)))
          if (!is_promotion_dup) record(e, "error")
        }
      ),
      warning = function(w) {
        if (getOption("warn") >= 2) {
          # R will promote this to an error and halt; record it now, preserving
          # the original warning classes, and let promotion proceed (no muffle).
          record(w, "error")
          promoted_msgs <<- c(promoted_msgs, conditionMessage(w))
        } else {
          record(w, "warning")
          invokeRestart("muffleWarning")
        }
      },
      message = function(m) {
        if (!inherits(m, "packageStartupMessage")) record(m, "message")
        invokeRestart("muffleMessage")
      }
    )
    list(observed = TRUE, conditions = conditions)
  }

  if (backend == "inprocess") {
    # A script may call options()/set.seed()/etc.; isolate global state so it
    # cannot leak into the caller's session (new.env does NOT isolate options).
    old_opts <- options()
    on.exit(options(old_opts), add = TRUE)
    return(runner(statements))
  }

  # In a separate process, a timeout or hard crash means we could not observe
  # the run: observed = FALSE (distinct from "ran cleanly").
  tryCatch(
    callr::r(runner, args = list(statements = statements), timeout = timeout),
    error = function(e) list(observed = FALSE, conditions = list())
  )
}
