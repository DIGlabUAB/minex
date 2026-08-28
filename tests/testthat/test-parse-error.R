test_that("unparseable input raises a classed minex_parse_error", {
  expect_error(
    minex(code = "f <- function( {", backend = "inprocess"),
    class = "minex_parse_error"
  )
})

test_that("minex_parse_error inherits minex_error", {
  e <- tryCatch(minex(code = "f <- function( {", backend = "inprocess"),
                error = function(e) e)
  expect_s3_class(e, "minex_error")
})

test_that("the classed error preserves the underlying parser message", {
  e <- tryCatch(minex(code = "if (x % 2 == 0) TRUE", backend = "inprocess"),
                minex_parse_error = function(e) e)
  expect_true(nzchar(e$parse_error))
  expect_match(conditionMessage(e), "parseable")
  # The original parser diagnostic must survive, not be swallowed.
  expect_match(conditionMessage(e), "unexpected")
})

test_that("parseable failing input is unaffected", {
  expect_error(
    minex(code = c("x <- 1", "stop('boom')"), backend = "inprocess"),
    NA
  )
})
