#' @keywords internal
#' @noRd
build_matcher <- function(match) {
  if (is.function(match)) {
    return(match)
  }
  match <- match.arg(match, c("message", "class", "both"))
  message_ok <- function(candidate, target) identical(candidate$message, target$message)
  class_ok   <- function(candidate, target) length(intersect(candidate$classes, target$classes)) > 0L
  switch(match,
    message = message_ok,
    class   = class_ok,
    both    = function(candidate, target) message_ok(candidate, target) &&
                                          class_ok(candidate, target)
  )
}
