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