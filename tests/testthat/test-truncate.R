cs <- function(type, message, stmt_index, classes = type) {
  list(type = type, message = message, classes = classes, stmt_index = stmt_index)
}
msg_match <- build_matcher("message")

test_that("error target picks the erroring statement", {
  conds <- list(cs("warning", "w", 1L), cs("error", "boom", 3L))
  target <- cs("error", "boom", 3L)
  idx <- pick_target_index(conds, target, msg_match, "error")
  expect_equal(idx, 3L)
})

test_that("warning target picks the LAST matching warning", {
  conds <- list(cs("warning", "w", 2L), cs("warning", "w", 5L))
  target <- cs("warning", "w", 5L)
  idx <- pick_target_index(conds, target, msg_match, "warning")
  expect_equal(idx, 5L)
})

test_that("any target uses highest severity present (error > warning)", {
  conds <- list(cs("warning", "w", 2L), cs("error", "boom", 4L))
  target <- cs("error", "boom", 4L)
  idx <- pick_target_index(conds, target, msg_match, "any")
  expect_equal(idx, 4L)
})

test_that("no matching condition yields NA", {
  conds <- list(cs("error", "boom", 3L))
  target <- cs("warning", "w", 1L)
  idx <- pick_target_index(conds, target, msg_match, "warning")
  expect_true(is.na(idx))
})

test_that("truncate_statements drops the tail, and NA is a no-op", {
  stmts <- c("a", "b", "c", "d")
  expect_equal(truncate_statements(stmts, 2L), c("a", "b"))
  expect_equal(truncate_statements(stmts, NA_integer_), stmts)
})
