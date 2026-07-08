test_that("minex strips statements irrelevant to the failure", {
  script <- c(
    "a <- 10",
    "b <- 20",
    "log('not a number')"
  )
  res <- minex(code = script, backend = "inprocess")
  expect_s3_class(res, "minex_result")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "log\\('not a number'\\)")
})

test_that("minex keeps statements the failure depends on", {
  script <- c(
    "x <- c(1, 2, NA)",
    "m <- mean(x)",
    "if (is.na(m)) stop('mean is NA')"
  )
  res <- minex(code = script, backend = "inprocess")
  # All three lines are required to reproduce the error.
  expect_equal(res$n_minimal, 3L)
})

test_that("minex errors when the input does not fail", {
  expect_error(
    minex(code = c("x <- 1", "y <- 2"), backend = "inprocess"),
    "nothing to minimize"
  )
})

test_that("minex requires either file or code", {
  expect_error(minex(), "Supply either")
})

test_that("minex reads from a file", {
  path <- tempfile(fileext = ".R")
  writeLines(c("ok <- TRUE", "stop('from file')"), path)
  on.exit(unlink(path), add = TRUE)
  res <- minex(file = path, backend = "inprocess")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "from file")
})

test_that("minex accepts a custom oracle", {
  script <- c("one <- 1", "two <- 2", "three <- 3")
  # Reproduce the "failure" of containing the assignment to `two`.
  res <- minex(
    code = script,
    oracle = function(stmts) any(grepl("two", stmts)),
    backend = "inprocess"
  )
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "two")
  expect_null(res$target)
})

test_that("minex validates a non-function oracle", {
  expect_error(
    minex(code = "stop('x')", oracle = 42, backend = "inprocess"),
    "must be a function"
  )
})

test_that("class matching tolerates a changed message", {
  # A custom-classed condition; the message varies but the class is stable.
  script <- c(
    "n <- 3",
    "cond <- structure(class = c('myError', 'error', 'condition'),",
    "                  list(message = paste('value', n), call = NULL))",
    "stop(cond)"
  )
  res <- minex(code = script, match = "class", backend = "inprocess")
  expect_true(res$n_minimal >= 1L)
})

test_that("print returns its input invisibly", {
  res <- minex(code = "stop('boom')", backend = "inprocess")
  expect_output(print(res), "minex_result")
  expect_invisible(print(res))
})

test_that("callr backend reproduces the in-process result", {
  skip_on_cran()
  skip_if_not_installed("callr")
  script <- c("a <- 1", "b <- 2", "stop('callr boom')")
  res <- minex(code = script, backend = "callr")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "callr boom")
})
