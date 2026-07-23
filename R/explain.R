#' @keywords internal
#' @noRd
new_minex_explanation <- function(raw, modes, model, bug_report, fix_verify) {
  want <- function(m) m %in% modes
  fix <- if (want("fix")) {
    list(code = raw$fix_code, rationale = raw$fix_rationale,
         verified = if (is.null(fix_verify)) NA else fix_verify$verified,
         verification = if (is.null(fix_verify)) NA_character_ else fix_verify$verification)
  } else NULL
  structure(list(
    explanation = if (want("explain")) raw$explanation else NULL,
    diagnosis = if (want("diagnose")) list(category = raw$diagnosis_category,
                                           concept = raw$diagnosis_concept,
                                           severity = raw$diagnosis_severity) else NULL,
    fix = fix,
    bug_report = if (want("report")) bug_report else NULL,
    model = model,
    modes = modes
  ), class = "minex_explanation")
}

#' @export
as.character.minex_explanation <- function(x, ...) {
  if (is.null(x$bug_report)) "" else x$bug_report
}

#' @export
format.minex_explanation <- function(x, ...) {
  as.character(x)
}

#' @export
print.minex_explanation <- function(x, ...) {
  if (!is.null(x$explanation)) {
    cat("WHY IT FAILS\n  ", x$explanation, "\n\n", sep = "")
  }
  if (!is.null(x$fix)) {
    tag <- if (isTRUE(x$fix$verified)) "verified" else
           if (isFALSE(x$fix$verified)) "NOT verified" else "unverified"
    cat(sprintf("FIX  (%s: %s)\n  %s\n\n", tag,
                if (is.na(x$fix$verification)) "" else x$fix$verification,
                x$fix$code))
  }
  if (!is.null(x$diagnosis)) {
    cat(sprintf("TYPE\n  %s / %s\n\n", x$diagnosis$category, x$diagnosis$concept))
  }
  if (!is.null(x$bug_report)) {
    cat("BUG REPORT\n"); cat(x$bug_report, "\n")
  }
  invisible(x)
}