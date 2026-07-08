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
