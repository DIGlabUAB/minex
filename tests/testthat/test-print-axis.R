mk_axis_result <- function(n_original, n_minimal, n_chars_original,
                           n_chars_minimal, granularity = "statement") {
  structure(list(
    code = "stop('boom')", original = c("x <- 1", "stop('boom')"),
    n_original = n_original, n_minimal = n_minimal, oracle_calls = 7L,
    target = list(message = "boom", classes = "simpleError",
                  conditions = list(), failing_index = 2L),
    match = "message", backend = "callr", complete = TRUE,
    algorithm = "cdd", condition = "error", max_oracle_calls = Inf,
    trace = NULL, granularity = granularity,
    n_chars_original = n_chars_original, n_chars_minimal = n_chars_minimal
  ), class = "minex_result")
}

test_that("print leads with statements when the statement count moved", {
  out <- capture.output(print(mk_axis_result(3L, 1L, 100L, 40L)))
  expect_match(out[1], "3 statement\\(s\\) reduced to 1")
})

test_that("print leads with characters when only characters moved", {
  out <- capture.output(print(
    mk_axis_result(3L, 3L, 1089L, 572L, granularity = "expression")))
  expect_match(out[1], "1089 -> 572 characters")
  expect_match(out[1], "3 statement\\(s\\) unchanged")
})

test_that("the character figure is not reported twice", {
  out <- capture.output(print(
    mk_axis_result(3L, 3L, 1089L, 572L, granularity = "expression")))
  expect_equal(sum(grepl("1089 -> 572", out)), 1L)
})

test_that("print hints at expression granularity when nothing moved at all", {
  out <- capture.output(print(mk_axis_result(3L, 3L, 500L, 500L)))
  expect_match(out[1], "3 statement\\(s\\) reduced to 3")
  expect_true(any(grepl("nothing removable at statement granularity", out)))
  expect_true(any(grepl('granularity = "expression"', out, fixed = TRUE)))
})

test_that("no spurious hint when the result did reduce", {
  out <- capture.output(print(mk_axis_result(3L, 1L, 100L, 40L)))
  expect_false(any(grepl("nothing removable", out)))
})
