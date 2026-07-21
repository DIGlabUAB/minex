mk_result <- function(complete = TRUE) {
  structure(list(
    code = "stop('boom')", original = c("x <- 1", "stop('boom')"),
    n_original = 2L, n_minimal = 1L, oracle_calls = 4L,
    target = list(message = "boom", classes = "simpleError",
                  conditions = list(), failing_index = 2L),
    match = "message", backend = "callr", complete = complete,
    algorithm = "cdd", condition = "error", max_oracle_calls = Inf, trace = NULL
  ), class = "minex_result")
}

test_that("complete result prints without an incomplete note", {
  expect_output(print(mk_result(TRUE)), "reduced to 1")
  expect_output(print(mk_result(TRUE)), "boom")
})

test_that("incomplete result prints an incomplete header and reducible note", {
  out <- capture.output(print(mk_result(FALSE)))
  expect_true(any(grepl("incomplete", out)))
  expect_true(any(grepl("may still be reducible", out)))
})

test_that("print returns its input invisibly", {
  expect_invisible(print(mk_result()))
})
