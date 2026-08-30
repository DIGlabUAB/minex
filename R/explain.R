#' @keywords internal
#' @noRd
new_minex_explanation <- function(raw, modes, model, bug_report, fix_verify) {
  want <- function(m) m %in% modes
  if (want("fix")) {
    fix <- list(code = raw$fix_code, rationale = raw$fix_rationale,
                verified = if (is.null(fix_verify)) NA else fix_verify$verified,
                verification = if (is.null(fix_verify)) NA_character_ else fix_verify$verification)
  } else {
    fix <- NULL
  }
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
    tag <- if (isTRUE(x$fix$verified)) "verified" else if (isFALSE(x$fix$verified)) "NOT verified" else "unverified"
    cat(sprintf("FIX  (%s: %s)\n  %s\n\n", tag,
                if (is.na(x$fix$verification)) "" else x$fix$verification,
                x$fix$code))
  }
  if (!is.null(x$diagnosis)) {
    cat(sprintf("TYPE\n  %s / %s\n\n", x$diagnosis$category, x$diagnosis$concept))
  }
  if (!is.null(x$bug_report)) {
    cat("BUG REPORT\n", x$bug_report, "\n", sep = "")
  }
  invisible(x)
}

#' @keywords internal
#' @noRd
resolve_chat <- function(chat) {
  if (is.null(chat)) chat <- getOption("minex.chat")
  if (is.null(chat)) {
    stop("No LLM chat available. Pass `chat = ellmer::chat_ollama(...)` (or set ",
         "`options(minex.chat = ...)`). See ?explain_failure.", call. = FALSE)
  }
  # Use statelessly: clone so turn history never accumulates across calls.
  if (is.function(chat$clone)) chat$clone() else chat
}

#' @keywords internal
#' @noRd
build_bug_report <- function(x, raw, session_info) {
  lines <- c(
    "Minimal reproducible example:",
    "```r",
    x$code,
    sprintf("#> %s: %s", x$condition,
            if (is.null(x$target)) "custom oracle" else x$target$message),
    "```",
    "", raw$explanation)
  if (isTRUE(session_info)) {
    lines <- c(lines, "", paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
  }
  paste(lines, collapse = "\n")
}

#' Explain a minex failure with an LLM
#'
#' Hands a [minex()] result's minimal snippet to a language model and returns a
#' structured explanation: why it fails, a fix (optionally executed to verify it
#' is gone), a paste-ready bug report, and a diagnosis. Opt-in; requires the
#' suggested `ellmer` package and a configured chat backend.
#'
#' **Executing model output:** with `verify_fix = TRUE` (default) the proposed
#' fix is run in a fresh `callr` process with a timeout. `callr` isolates R
#' session state, not the OS; a fix may still have side effects. Use
#' `verify_fix = FALSE` for input you would not run yourself.
#'
#' Prompt-level data framing and the source-text call denylist are
#' defense-in-depth mitigations, not security boundaries. Code supplied to the
#' model can influence its output, and aliases or dynamic evaluation can evade
#' the denylist. See the OWASP Prompt Injection guidance in the references.
#'
#' @param x A `minex_result` from [minex()].
#' @param chat An `ellmer` Chat object (e.g. `ellmer::chat_ollama()`). Defaults to
#'   `getOption("minex.chat")`. Used statelessly (cloned per call).
#' @param verify_fix Logical. Run the proposed fix to check the failure is gone.
#' @param timeout Seconds allowed for fix execution in the isolated R process.
#' @param session_info Logical. Include `utils::sessionInfo()` in the bug report.
#' @param modes Which sections to produce.
#' @return A `minex_explanation` object.
#' @references
#' OWASP Foundation. "LLM01:2025 Prompt Injection."
#' \url{https://genai.owasp.org/llmrisk/llm01-prompt-injection/}
#' @examples
#' \dontrun{
#' # Write a failing script and reduce it, then explain the failure.
#' tmp <- tempfile(fileext = ".R")
#' writeLines("x <- c(1, 2, NA)\nif (is.na(mean(x))) stop('mean is NA')", tmp)
#' res <- minex(tmp, backend = "inprocess")
#' # Requires a configured chat backend (e.g. ellmer::chat_ollama()).
#' explain_failure(res, chat = ellmer::chat_ollama(model = "llama3.2"),
#'                 verify_fix = FALSE)
#' }
#' @export
explain_failure <- function(x, chat = NULL, verify_fix = TRUE, timeout = 60,
                            session_info = TRUE,
                            modes = c("explain", "fix", "report", "diagnose")) {
  if (!inherits(x, "minex_result")) {
    stop("`x` must be a minex_result (the output of minex()).", call. = FALSE)
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("explain_failure() needs the 'ellmer' package. Install it with ",
         "install.packages('ellmer').", call. = FALSE)
  }
  modes <- match.arg(modes, several.ok = TRUE)
  chat <- resolve_chat(chat)

  prompt <- build_prompt(x, modes, session_info)
  raw <- chat$chat_structured(prompt, type = explanation_type())

  fix_verify <- NULL
  if (isTRUE(verify_fix) && "fix" %in% modes) {
    fix_verify <- verify_fix_code(raw$fix_code, x$target, x$condition, x$match,
                                  backend = "callr", timeout = timeout)
  }
  bug_report <- if ("report" %in% modes) build_bug_report(x, raw, session_info) else NULL
  model <- if (is.function(chat$get_model)) chat$get_model() else NA_character_

  new_minex_explanation(raw, modes = modes, model = model,
                        bug_report = bug_report, fix_verify = fix_verify)
}
