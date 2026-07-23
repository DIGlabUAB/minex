test_that("explanation_type builds an ellmer type object", {
  skip_if_not_installed("ellmer")
  ty <- explanation_type()
  expect_true(inherits(ty, "ellmer::TypeObject"))
})

fake_result <- function(session_info_present = TRUE) {
  structure(list(
    code = c("x <- c(1, 2, NA)", "if (is.na(mean(x))) stop('mean is NA')"),
    target = list(message = "mean is NA", classes = c("simpleError","error","condition")),
    condition = "error", match = "message", backend = "callr"
  ), class = "minex_result")
}

test_that("build_prompt embeds code + error and frames code as data", {
  p <- build_prompt(fake_result(), modes = c("explain","fix","report","diagnose"),
                    session_info = FALSE)
  expect_match(p, "mean is NA")
  expect_match(p, "x <- c\\(1, 2, NA\\)")
  expect_match(p, "data to analyze")  # injection framing present
})

test_that("build_prompt honors session_info", {
  with <- build_prompt(fake_result(), c("report"), session_info = TRUE)
  without <- build_prompt(fake_result(), c("report"), session_info = FALSE)
  expect_true(grepl("R version", with))
  expect_false(grepl("R version", without))
})