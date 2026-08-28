raw <- list(
  explanation = "mean() returns NA when x has NA.",
  fix_code = "m <- mean(x, na.rm = TRUE)", fix_rationale = "Skip NA.",
  diagnosis_category = "Runtime error", diagnosis_concept = "missing values",
  diagnosis_severity = "error"
)

test_that("new_minex_explanation maps fields and nulls unrequested modes", {
  ex <- new_minex_explanation(raw, modes = c("explain","diagnose"),
                              model = "stub", bug_report = "REPORT",
                              fix_verify = NULL)
  expect_s3_class(ex, "minex_explanation")
  expect_equal(ex$explanation, raw$explanation)
  expect_null(ex$fix)          # fix not requested
  expect_equal(ex$diagnosis$category, "Runtime error")
})

test_that("as.character returns the bug report", {
  ex <- new_minex_explanation(raw, modes = "report", model = "stub",
                              bug_report = "THE REPORT", fix_verify = NULL)
  expect_equal(as.character(ex), "THE REPORT")
})

test_that("print is stable and mentions the fix verification", {
  ex <- new_minex_explanation(raw, modes = c("explain","fix"), model = "stub",
                              bug_report = "R",
                              fix_verify = list(verified = TRUE, verification = "clean run"))
  expect_output(print(ex), "WHY IT FAILS")
  expect_output(print(ex), "verified")
})

# A duck-typed stub Chat: implements chat_structured() + a model accessor.
stub_chat <- function(payload) {
  structure(list(
    chat_structured = function(prompt, type) payload,
    get_model = function() "stub-model",
    clone = function() stub_chat(payload)
  ), class = "stub_chat")
}

test_that("explain_failure errors on a non-minex_result", {
  expect_error(explain_failure(list(1,2), chat = stub_chat(raw)), "minex_result")
})

test_that("explain_failure runs the full pipeline with a stub chat", {
  skip_if_not_installed("ellmer")   # explanation_type() builds a real ellmer type
  res <- structure(list(
    code = "if (is.na(mean(c(1,NA)))) stop('mean is NA')",
    target = list(message = "mean is NA", classes = c("simpleError","error","condition"),
                  conditions = list(list(type="error", message="mean is NA",
                                         classes=c("simpleError","error","condition"),
                                         stmt_index=1L))),
    condition = "error", match = "message", backend = "inprocess"
  ), class = "minex_result")
  ex <- explain_failure(res, chat = stub_chat(raw), verify_fix = FALSE)
  expect_s3_class(ex, "minex_explanation")
  expect_equal(ex$explanation, raw$explanation)
  expect_true("mean is NA" %in% strsplit(ex$bug_report, "\n")[[1]] |
              grepl("mean is NA", ex$bug_report))
})

test_that("explain_failure with no chat and no option errors with guidance", {
  res <- structure(list(code="stop('x')", target=NULL, condition="error",
                        match="message", backend="callr"), class="minex_result")
  old <- getOption("minex.chat"); options(minex.chat = NULL); on.exit(options(minex.chat = old))
  expect_error(explain_failure(res), "chat|minex.chat|ellmer")
})

test_that("explain_failure live round-trip (skipped without a configured model)", {
  skip_on_cran()
  skip_if_not_installed("ellmer")
  skip_if(is.null(getOption("minex.chat")), "no minex.chat configured")
  res <- minex(code = c("x <- c(1, 2, NA)", "if (is.na(mean(x))) stop('mean is NA')"),
               backend = "inprocess")
  ex <- explain_failure(res, verify_fix = FALSE)
  expect_s3_class(ex, "minex_explanation")
  expect_true(nchar(ex$explanation) > 0)
})