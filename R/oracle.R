#' @keywords internal
#' @noRd
run_code <- function(code, backend = c("callr", "inprocess"), timeout = 60) {
  backend <- match.arg(backend)
  statements <- code

  runner <- function(statements) {
    env <- new.env(parent = globalenv())
    conditions <- list()
    promoted_msgs <- list() # warnings already recorded via warn=2 promotion
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

    byte_prefix <- function(text, limit) {
      native <- enc2native(text)
      if (nchar(native, type = "bytes") <= limit) return(native)
      chars <- strsplit(native, "", fixed = TRUE)[[1L]]
      kept <- cumsum(nchar(chars, type = "bytes")) <= limit
      paste0(chars[kept], collapse = "")
    }

    promotion_summary <- function(w) {
      message <- conditionMessage(w)
      format <- gettext("(converted from warning) %s", domain = "R")
      messages <- sprintf(format, message)
      # R truncates warning text at warning.length bytes without splitting a
      # multibyte character, then appends a translated truncation marker.
      native <- enc2native(message)
      limit <- getOption("warning.length")
      if (nchar(native, type = "bytes") > limit) {
        truncated <- paste0(byte_prefix(native, limit), " ",
                            gettext("[... truncated]", domain = "R"))
        messages <- c(messages, sprintf(format, truncated))
      }
      # R's simple-error signaling has an additional 8190-byte payload limit.
      # Keep exact bounded forms, rather than accepting arbitrary prefixes.
      messages <- unique(c(messages, vapply(messages, byte_prefix,
                                            character(1), limit = 8190L)))
      list(messages = messages, call = conditionCall(w))
    }

    stopped_condition <- function() {
      # stop() accepts conditions without an "error" class. An exiting
      # condition handler would also intercept ordinary signalCondition() and
      # unwind code that should continue, so inspect the actual signaling
      # function and leave warnings/messages in stop's arguments alone.
      signallers <- list(base::stop, base::warning, base::message,
                        base::signalCondition)
      for (frame in rev(seq_len(sys.nframe()))) {
        fn <- sys.function(frame)
        if (any(vapply(signallers, identical, logical(1), fn))) {
          return(identical(fn, base::stop))
        }
      }
      FALSE
    }

    withRestarts(withCallingHandlers(
      tryCatch(
        for (i in seq_along(statements)) {
          current <- i
          eval(parse(text = statements[[i]])[[1]], envir = env)
        },
        error = function(e) {
          # Under warn>=2 a warning is promoted to an error whose message is the
          # warning text with a "(converted from warning) " prefix; the warning
          # handler already recorded it, so skip that duplicate (a class-stripped
          # re-entry). Match exact translated messages and originating calls: a
          # single top-level statement (e.g. a `{...}` block) may recover the
          # promotion via its own tryCatch and then raise a genuinely different
          # error, which shares the index and must still be kept.
          em <- conditionMessage(e)
          is_promotion_dup <- inherits(e, "simpleError") &&
            any(vapply(promoted_msgs, function(p) {
              em %in% p$messages && identical(conditionCall(e), p$call)
            }, logical(1)))
          if (!is_promotion_dup) record(e, "error")
        }
      ),
      condition = function(cond) {
        if (!inherits(cond, "error") && stopped_condition()) {
          record(cond, "error")
          invokeRestart("minex_stop_run")
        }
      },
      warning = function(w) {
        if (getOption("warn") >= 2) {
          # R will promote this to an error and halt; record it now, preserving
          # the original warning classes, and let promotion proceed (no muffle).
          record(w, "error")
          promoted_msgs[[length(promoted_msgs) + 1L]] <<- promotion_summary(w)
        } else {
          record(w, "warning")
          invokeRestart("muffleWarning")
        }
      },
      message = function(m) {
        if (!inherits(m, "packageStartupMessage")) record(m, "message")
        invokeRestart("muffleMessage")
      }
    ), minex_stop_run = function() NULL)
    list(observed = TRUE, conditions = conditions, failure = NULL)
  }

  if (backend == "inprocess") {
    # new.env() does not isolate options. Restore the complete option snapshot;
    # other side effects (including RNG and working directory) are not isolated.
    old_opts <- options()
    on.exit({
      added_opts <- setdiff(names(options()), names(old_opts))
      if (length(added_opts)) {
        options(stats::setNames(rep(list(NULL), length(added_opts)), added_opts))
      }
      options(old_opts)
    }, add = TRUE)
    return(runner(statements))
  }

  # In a separate process, a timeout or hard crash means we could not observe
  # the run: observed = FALSE (distinct from "ran cleanly").
  tryCatch(
    callr::r(runner, args = list(statements = statements), timeout = timeout),
    error = function(e) {
      type <- if (inherits(e, "callr_timeout_error")) {
        "timeout"
      } else if (inherits(e, "callr_status_error")) {
        "process_failure"
      } else {
        "backend_error"
      }
      list(observed = FALSE, conditions = list(), failure = list(
        type = type, message = conditionMessage(e), classes = class(e), cause = e
      ))
    }
  )
}

#' @keywords internal
#' @noRd
stop_unobserved_evaluation <- function(result, backend, stage) {
  failure <- result$failure %||% list(
    type = "unknown", message = "No backend diagnostic was available.",
    classes = character(), cause = NULL
  )
  stop(structure(
    list(message = sprintf(
      "The %s evaluation could not be observed with backend '%s' (%s): %s",
      stage, backend, failure$type, failure$message
    ), call = NULL, backend = backend, stage = stage,
    failure = failure, parent = failure$cause),
    class = c("minex_evaluation_error", "error", "condition")
  ))
}
