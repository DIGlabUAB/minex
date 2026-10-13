mk <- function(message, classes) list(message = message, classes = classes)

test_that("message matcher compares messages exactly", {
  m <- build_matcher("message")
  expect_true(m(mk("boom", "simpleError"), mk("boom", "x")))
  expect_false(m(mk("boom", "e"), mk("bang", "e")))
})

test_that("class matcher checks shared specific condition classes", {
  m <- build_matcher("class")
  expect_true(m(mk("a", c("myErr", "error")), mk("b", c("myErr", "error"))))
  expect_false(m(mk("a", "simpleError"), mk("b", "otherError")))
  expect_false(m(mk("a", c("simpleError", "error", "condition")),
                 mk("b", c("myErr", "error", "condition"))))
  expect_false(m(mk("a", c("otherErr", "simpleError", "error")),
                 mk("a", c("myErr", "simpleError", "error"))))
  expect_true(m(mk("a", c("childErr", "domainErr", "error")),
                mk("b", c("otherChildErr", "domainErr", "error"))))
})

test_that("generic class matching requires the same message and compatible class", {
  m <- build_matcher("class")
  generic_error <- c("simpleError", "error", "condition")
  expect_true(m(mk("boom", generic_error), mk("boom", generic_error)))
  expect_false(m(mk("bang", generic_error), mk("boom", generic_error)))
  expect_false(m(mk("boom", c("simpleWarning", "warning", "condition")),
                 mk("boom", generic_error)))
  expect_true(m(mk("boom", c("myErr", "error", "condition")),
                mk("boom", generic_error)))
  expect_true(m(mk("boom", "condition"), mk("boom", "condition")))
  expect_false(m(mk("bang", "condition"), mk("boom", "condition")))
})

test_that("both matcher requires message AND class", {
  m <- build_matcher("both")
  expect_true(m(mk("boom", "myErr"), mk("boom", "myErr")))
  expect_false(m(mk("boom", "myErr"), mk("boom", "otherErr")))
  expect_false(m(mk("boom", "myErr"), mk("bang", "myErr")))
  expect_false(m(mk("boom", c("simpleError", "error")),
                 mk("boom", c("myErr", "error"))))
})

test_that("a user function is passed through unchanged", {
  f <- function(candidate, target) TRUE
  expect_identical(build_matcher(f), f)
})

test_that("an invalid string errors like match.arg", {
  expect_error(build_matcher("nonsense"))
})
