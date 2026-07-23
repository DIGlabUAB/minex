#' @keywords internal
#' @noRd
explanation_type <- function() {
  ellmer::type_object(
    explanation = ellmer::type_string(
      "Plain-English explanation of why the snippet fails."),
    fix_code = ellmer::type_string(
      "A corrected version of the snippet as runnable R code, or empty if none."),
    fix_rationale = ellmer::type_string(
      "Why the fix works."),
    diagnosis_category = ellmer::type_string(
      "Short failure category, e.g. 'Runtime error'."),
    diagnosis_concept = ellmer::type_string(
      "The R concept involved, e.g. 'missing-value handling'."),
    diagnosis_severity = ellmer::type_string(
      "One of 'error', 'warning', 'message'.")
  )
}