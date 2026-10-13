#' @keywords internal
#' @noRd
build_matcher <- function(match) {
  if (is.function(match)) {
    return(match)
  }
  match <- match.arg(match, c("message", "class", "both"))
  message_ok <- function(candidate, target) identical(candidate$message, target$message)
  generic_classes <- c("condition", "error", "warning", "message",
                       "simpleError", "simpleWarning", "simpleMessage")
  class_ok <- function(candidate, target) {
    specific <- setdiff(target$classes, generic_classes)
    if (length(specific) > 0L) {
      # Severity/root classes and the default simple classes cannot establish
      # that a custom failure survived: nearly all unrelated errors share them.
      return(length(intersect(candidate$classes, specific)) > 0L)
    }
    # A generic condition has no specific class to distinguish its cause.
    # Retain message equality, with a shared non-root class when available,
    # rather than treating every ordinary error as the same failure.
    generic <- setdiff(target$classes, "condition")
    compatible <- length(generic) == 0L ||
      length(intersect(candidate$classes, generic)) > 0L
    compatible && message_ok(candidate, target)
  }
  switch(match,
    message = message_ok,
    class   = class_ok,
    both    = function(candidate, target) message_ok(candidate, target) &&
                                          class_ok(candidate, target)
  )
}
