test_that("is_denylisted flags side-effecting calls and clears benign code", {
  expect_true(is_denylisted("system('rm -rf /')"))
  expect_true(is_denylisted("x <- 1\ndownload.file(u, 'f')"))
  expect_true(is_denylisted("pipe('sh')"))
  expect_true(is_denylisted("writeLines(x, '~/.bashrc')"))
  expect_false(is_denylisted("m <- mean(x, na.rm = TRUE)"))
  expect_false(is_denylisted("y <- sqrt(sum(1:10))"))
})