test_that("PROBDD_GROWTH_RATE is 1/(1-exp(-1))", {
  expect_equal(PROBDD_GROWTH_RATE, 1 / (1 - exp(-1)))
})

test_that("cdd_reduce isolates a single required element", {
  test <- function(idx) 4L %in% idx
  out <- cdd_reduce(1:8, test)
  expect_true(4L %in% out)
  expect_true(length(out) < 8L)
})

test_that("cdd_reduce keeps jointly required elements", {
  test <- function(idx) all(c(2L, 7L) %in% idx)
  out <- cdd_reduce(1:8, test)
  expect_true(all(c(2L, 7L) %in% out))
})

test_that("cdd_reduce terminates on a singleton-required set", {
  test <- function(idx) length(idx) >= 1L
  out <- cdd_reduce(1:5, test)
  expect_true(length(out) >= 1L)
})
