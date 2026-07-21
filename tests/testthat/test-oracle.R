test_that("run_code captures a warning then an error with indices", {
  code <- c("warning('w1')", "x <- 1", "stop('boom')")
  res <- run_code(code, backend = "inprocess")
  expect_true(res$observed)
  types <- vapply(res$conditions, `[[`, character(1), "type")
  expect_equal(types, c("warning", "error"))
  expect_equal(res$conditions[[1]]$stmt_index, 1L)
  expect_equal(res$conditions[[2]]$stmt_index, 3L)
  expect_equal(res$conditions[[2]]$message, "boom")
})

test_that("run_code records a clean run as observed with no conditions", {
  res <- run_code(c("x <- 1", "y <- 2"), backend = "inprocess")
  expect_true(res$observed)
  expect_length(res$conditions, 0L)
})

test_that("run_code filters packageStartupMessage from message capture", {
  code <- c("packageStartupMessage('startup')", "message('real')")
  res <- run_code(code, backend = "inprocess")
  msgs <- Filter(function(c) c$type == "message", res$conditions)
  expect_length(msgs, 1L)
  expect_match(msgs[[1]]$message, "real")
})

test_that("run_code under warn=2 records ONE error carrying warning classes", {
  code <- c("options(warn = 2)",
            "warning(structure(class = c('myWarn','warning','condition'),
                               list(message = 'w', call = NULL)))",
            "message('never runs')")
  res <- run_code(code, backend = "inprocess")
  # Exactly one promoted condition, not a duplicate stripped simpleError.
  promoted <- Filter(function(c) c$stmt_index == 2L, res$conditions)
  expect_length(promoted, 1L)
  expect_true("myWarn" %in% promoted[[1]]$classes)
})

test_that("run_code ignores conditions the script catches itself", {
  code <- c("try(stop('caught'), silent = TRUE)", "stop('real')")
  res <- run_code(code, backend = "inprocess")
  msgs <- vapply(res$conditions, `[[`, character(1), "message")
  expect_false("caught" %in% msgs)
  expect_true("real" %in% msgs)
})

test_that("inprocess run_code does not leak options() into the session", {
  before <- getOption("warn")
  run_code(c("options(warn = 2)", "x <- 1"), backend = "inprocess")
  expect_identical(getOption("warn"), before)
})
