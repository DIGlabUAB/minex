test_that("ddmin isolates the single required element", {
  words <- c("the", "quick", "brown", "fox")
  result <- ddmin(words, function(s) "fox" %in% s)
  expect_identical(result, "fox")
})

test_that("ddmin keeps all jointly required elements", {
  nums <- 1:6
  # The predicate needs both the element 6 and a sum of at least 11.
  result <- ddmin(nums, function(s) 6 %in% s && sum(s) >= 11)
  expect_true(6 %in% result)
  expect_gte(sum(result), 11)
  # One-minimality: removing any element breaks the predicate.
  for (i in seq_along(result)) {
    expect_false(6 %in% result[-i] && sum(result[-i]) >= 11)
  }
})

test_that("ddmin result is order-preserving", {
  x <- c(5L, 1L, 4L, 2L, 3L)
  result <- ddmin(x, function(s) length(s) >= 1L)
  expect_equal(result, result[order(match(result, x))])
})

test_that("ddmin errors when the full set is not interesting", {
  expect_error(
    ddmin(1:5, function(s) FALSE),
    "nothing to minimize"
  )
})

test_that("ddmin validates its predicate argument", {
  expect_error(ddmin(1:5, "not a function"), "must be a function")
})

test_that("ddmin handles the empty input", {
  expect_identical(ddmin(integer(0), function(s) TRUE), integer(0))
})

test_that("ddmin does not re-evaluate identical configurations", {
  calls <- 0L
  counting <- function(s) {
    calls <<- calls + 1L
    "fox" %in% s
  }
  ddmin(c("the", "quick", "brown", "fox"), counting)
  before <- calls
  # A second identical run uses a fresh cache, so call counts should match.
  calls <- 0L
  ddmin(c("the", "quick", "brown", "fox"), counting)
  expect_equal(calls, before)
})

test_that("ddmin return is a plain vector with no attributes", {
  out <- ddmin(c("the", "quick", "brown", "fox"), function(s) "fox" %in% s)
  expect_null(attributes(out))
  expect_identical(out, "fox")
})

test_that("ddmin populates the .info environment", {
  e <- new.env()
  ddmin(1:6, function(s) 6 %in% s, .info = e)
  expect_true(is.numeric(e$oracle_calls))
  expect_true(isTRUE(e$complete))
})

test_that("cdd and ddmin algorithms agree: both one-minimal AND equal size", {
  set.seed(NULL)
  interesting <- function(s) 4L %in% s
  a <- ddmin(1:9, interesting, algorithm = "cdd")
  b <- ddmin(1:9, interesting, algorithm = "ddmin")
  is_one_minimal <- function(x) all(vapply(seq_along(x),
    function(i) !isTRUE(interesting(x[-i])), logical(1)))
  expect_true(is_one_minimal(a))
  expect_true(is_one_minimal(b))
  expect_equal(length(a), length(b))
})

test_that("budget exhaustion sets complete = FALSE and still reproduces", {
  e <- new.env()
  interesting <- function(s) all(c(2L, 8L) %in% s)
  out <- ddmin(1:10, interesting, max_oracle_calls = 3L, .info = e)
  expect_false(e$complete)
  expect_true(isTRUE(interesting(out)))   # never returns an unverified set
})

test_that("max_oracle_calls < 1 errors rather than returning unverified", {
  expect_error(
    ddmin(1:5, function(s) length(s) > 0, max_oracle_calls = 0L),
    "max_oracle_calls"
  )
})

test_that("verbose = 'trace' populates a trace data frame with phase labels", {
  e <- new.env()
  ddmin(1:6, function(s) 6 %in% s, verbose = "trace", .info = e)
  expect_s3_class(e$trace, "data.frame")
  expect_true(all(c("call","phase","size","kept","cached","event") %in%
                    names(e$trace)))
  expect_true(all(e$trace$phase %in% c("reduce", "verify")))
})

test_that("verbose = 'trace' marks the budget-exhausted row", {
  e <- new.env()
  ddmin(1:10, function(s) all(c(2L, 8L) %in% s),
        max_oracle_calls = 3L, verbose = "trace", .info = e)
  expect_true("budget_exhausted" %in% e$trace$event)
})

test_that("verbose = TRUE emits phase messages", {
  expect_message(
    ddmin(1:6, function(s) 6 %in% s, verbose = TRUE),
    "reduce phase"
  )
})
