#' @keywords internal
#' @noRd
verbose_enabled <- function(verbose) !isFALSE(verbose)

#' @keywords internal
#' @noRd
wants_trace <- function(verbose) identical(verbose, "trace")

# Assemble the accumulated trace rows into a data frame (or NULL if unused).
#' @keywords internal
#' @noRd
build_trace <- function(rows) {
  if (length(rows) == 0L) {
    return(NULL)
  }
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

# A context belongs to one reduction, including its expression passes. Cache
# keys serialize vectors so different statement boundaries remain distinct.
#' @keywords internal
#' @noRd
validate_oracle_budget <- function(max_oracle_calls) {
  if (!is.numeric(max_oracle_calls) || length(max_oracle_calls) != 1L ||
      is.na(max_oracle_calls) || max_oracle_calls < 1 ||
      (is.finite(max_oracle_calls) && max_oracle_calls != floor(max_oracle_calls))) {
    stop("`max_oracle_calls` must be a numeric scalar at least 1 (a whole number or Inf).",
         call. = FALSE)
  }
  invisible(max_oracle_calls)
}

#' @keywords internal
#' @noRd
new_oracle_context <- function(max_oracle_calls = Inf, verbose = FALSE) {
  context <- new.env(parent = emptyenv())
  context$cache <- new.env(parent = emptyenv())
  context$calls <- 0L
  context$budget_calls <- 0L
  context$max_calls <- max_oracle_calls
  context$verbose <- verbose
  context$rows <- list()
  context$phase <- "reduce"
  context$size <- NULL
  context$stmt_index <- NA_integer_
  context$level <- NA_integer_
  context$owner <- NULL
  context
}

#' @keywords internal
#' @noRd
oracle_key <- function(value) {
  paste(as.character(serialize(value, NULL, version = 2L)), collapse = "")
}

#' @keywords internal
#' @noRd
oracle_context <- function(predicate) attr(predicate, "minex_oracle_context")

#' @keywords internal
#' @noRd
oracle_inherit <- function(predicate, context) {
  attr(predicate, "minex_oracle_context") <- context
  predicate
}

#' @keywords internal
#' @noRd
oracle_trace <- function(context, result, cached = FALSE, event = "") {
  if (wants_trace(context$verbose)) {
    context$rows[[length(context$rows) + 1L]] <- list(
      call = context$calls, phase = context$phase, size = context$size,
      kept = result, cached = cached, event = event,
      stmt_index = context$stmt_index, level = context$level)
  }
}

#' @keywords internal
#' @noRd
oracle_test <- function(context, key, evaluate, exempt = FALSE) {
  cached <- context$cache[[key]]
  if (!is.null(cached)) {
    oracle_trace(context, cached, cached = TRUE)
    return(cached)
  }
  if (!exempt && context$budget_calls >= context$max_calls) {
    oracle_trace(context, NA, event = "budget_exhausted")
    stop(structure(
      list(message = "oracle call budget exhausted", call = NULL),
      class = c("minex_oracle_limit_reached", "error", "condition")))
  }
  context$calls <- context$calls + 1L
  if (!exempt) context$budget_calls <- context$budget_calls + 1L
  result <- isTRUE(evaluate())
  assign(key, result, envir = context$cache)
  oracle_trace(context, result)
  result
}

#' @keywords internal
#' @noRd
oracle_wrap <- function(predicate, context) {
  force(predicate)
  force(context)
  oracle_inherit(function(value, .exempt = FALSE) {
    if (is.null(context$size)) context$size <- length(value)
    oracle_test(context, oracle_key(value), function() predicate(value), .exempt)
  }, context)
}

#' @keywords internal
#' @noRd
oracle_metadata <- function(context, phase, size, stmt_index = NA_integer_,
                            level = NA_integer_) {
  context$phase <- phase
  context$size <- size
  context$stmt_index <- stmt_index
  context$level <- level
  invisible(NULL)
}
