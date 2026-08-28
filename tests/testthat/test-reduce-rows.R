test_that("reduce_rows isolates the triggering row", {
  df <- data.frame(id = 1:6, value = c(3, 8, 999, 2, 5, 7))
  out <- reduce_rows(df, function(d) any(d$value > 100))
  expect_equal(nrow(out), 1L)
  expect_equal(out$value, 999)
})

test_that("reduce_rows preserves column structure and order", {
  df <- data.frame(a = 5:1, b = letters[1:5], stringsAsFactors = FALSE)
  # Reproducing requires both the first and the last row.
  out <- reduce_rows(df, function(d) all(c(5L, 1L) %in% d$a))
  expect_named(out, c("a", "b"))
  expect_equal(out$a, c(5L, 1L))
  expect_equal(out$b, c("a", "e"))
})

test_that("reduce_rows errors when the full data does not reproduce", {
  df <- data.frame(value = 1:5)
  expect_error(
    reduce_rows(df, function(d) any(d$value > 100)),
    "nothing to minimize"
  )
})

test_that("reduce_rows validates its arguments", {
  expect_error(reduce_rows(1:5, function(d) TRUE), "must be a data frame")
  expect_error(reduce_rows(data.frame(x = 1), 42), "must be a function")
})

test_that("reduce_rows returns empty input unchanged", {
  df <- data.frame(x = numeric(0))
  expect_equal(reduce_rows(df, function(d) TRUE), df)
})

test_that("reduce_rows accepts an algorithm argument", {
  df <- data.frame(id = 1:6, value = c(3, 8, 999, 2, 5, 7))
  out <- reduce_rows(df, function(d) any(d$value > 100), algorithm = "ddmin")
  expect_equal(nrow(out), 1L)
  expect_equal(out$value, 999)
})

test_that("reduce_rows returns a clean data frame (no leaked attributes)", {
  df <- data.frame(id = 1:6, value = c(3, 8, 999, 2, 5, 7))
  out <- reduce_rows(df, function(d) any(d$value > 100))
  expect_null(attr(out, "oracle_calls"))
  expect_null(attr(out, "complete"))
})
