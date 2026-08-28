# Severity order: error > warning > message.
.minex_severity <- c(message = 1L, warning = 2L, error = 3L)

#' @keywords internal
#' @noRd
pick_target_index <- function(conditions, target, matcher, condition) {
  if (length(conditions) == 0L) {
    return(NA_integer_)
  }
  if (condition == "any") {
    present <- vapply(conditions, `[[`, character(1), "type")
    condition <- names(which.max(.minex_severity[present]))
  }
  matches <- Filter(
    function(c) c$type == condition && isTRUE(matcher(c, target)),
    conditions
  )
  if (length(matches) == 0L) {
    return(NA_integer_)
  }
  # Last matching occurrence (for errors there is only one; for warnings the
  # last is the causally-latest safe truncation point).
  matches[[length(matches)]]$stmt_index
}

#' @keywords internal
#' @noRd
pick_target_condition <- function(conditions, condition) {
  if (length(conditions) == 0L) return(NULL)
  if (condition == "any") {
    present <- vapply(conditions, `[[`, character(1), "type")
    condition <- names(which.max(.minex_severity[present]))
  }
  matches <- Filter(function(c) c$type == condition, conditions)
  if (length(matches) == 0L) return(NULL)
  matches[[length(matches)]]
}

#' @keywords internal
#' @noRd
truncate_statements <- function(statements, failing_index) {
  if (is.na(failing_index)) {
    return(statements)
  }
  statements[seq_len(failing_index)]
}
