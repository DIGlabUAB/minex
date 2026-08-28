mk <- function(message, classes) list(message = message, classes = classes)

test_that("message matcher compares messages exactly", {
  m <- build_matcher("message")
  expect_true(m(mk("boom", "simpleError"), mk("boom", "x")))
  expect_false(m(mk("boom", "e"), mk("bang", "e")))
})

test_that("class matcher checks shared condition class", {
  m <- build_matcher("class")
  expect_true(m(mk("a", c("myErr", "error")), mk("b", c("myErr", "error"))))
  expect_false(m(mk("a", "simpleError"), mk("b", "otherError")))
})

test_that("both matcher requires message AND class", {
  m <- build_matcher("both")
  expect_true(m(mk("boom", "myErr"), mk("boom", "myErr")))
  expect_false(m(mk("boom", "myErr"), mk("boom", "otherErr")))
})

test_that("a user function is passed through unchanged", {
  f <- function(candidate, target) TRUE
  expect_identical(build_matcher(f), f)
})

test_that("an invalid string errors like match.arg", {
  expect_error(build_matcher("nonsense"))
})
