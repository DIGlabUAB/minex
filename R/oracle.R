#' @keywords internal
#' @noRd
condition_summary <- function(cond, type, stmt_index) {
  list(
    type       = type,
    message    = sub("\n$", "", conditionMessage(cond)),
    classes    = setdiff(class(cond), "condition"),
    stmt_index = as.integer(stmt_index)
  )
}

#' @keywords internal
#' @noRd
run_code <- function(code, backend = c("callr", "inprocess"), timeout = 60) {
  backend <- match.arg(backend)
  statements <- code

  runner <- function(statements) {
    env <- new.env(parent = globalenv())
    conditions <- list()
    promoted_idx <- integer(0)   # stmt indices already recorded via warn=2 promotion
    current <- 0L

    record <- function(cond, type) {
      conditions[[length(conditions) + 1L]] <<-
        condition_summary(cond, type, current)
    }

    withCallingHandlers(
      tryCatch(
        for (i in seq_along(statements)) {
          current <- i
          eval(parse(text = statements[[i]])[[1]], envir = env)
        },
        error = function(e) {
          # Skip the "(converted from warning)" error already recorded by the
          # warning handler under warn=2 (avoids a duplicate, class-stripped entry).
          if (!(current %in% promoted_idx)) record(e, "error")
        }
      ),
      warning = function(w) {
        if (getOption("warn") >= 2) {
          # R will promote this to an error and halt; record it now, preserving
          # the original warning classes, and let promotion proceed (no muffle).
          record(w, "error")
          promoted_idx <<- c(promoted_idx, current)
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
