test_that("is_denylisted flags side-effecting calls and clears benign code", {
  expect_true(is_denylisted("system('rm -rf /')"))
  expect_true(is_denylisted("x <- 1\ndownload.file(u, 'f')"))
  expect_true(is_denylisted("pipe('sh')"))
  expect_true(is_denylisted("writeLines(x, '~/.bashrc')"))
  expect_false(is_denylisted("m <- mean(x, na.rm = TRUE)"))
  expect_false(is_denylisted("y <- sqrt(sum(1:10))"))
})

test_that("target_type passes non-'any' through and resolves 'any' from recorded conditions", {
  # Recorded condition is an ERROR, so pass-through and 'any'-resolution DIFFER --
  # this kills a constant-return stub and forces both branches.
  target <- list(
    message = "boom", classes = c("simpleError", "error"),
    conditions = list(list(type = "error", message = "boom",
                           classes = c("simpleError", "error"), stmt_index = 1L))
  )
  expect_equal(target_type(target, "warning"), "warning")  # pass-through, unrelated to recorded
  expect_equal(target_type(target, "error"), "error")      # pass-through
  expect_equal(target_type(target, "any"), "error")        # resolved from the recorded condition
})