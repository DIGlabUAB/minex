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

test_that("run_code records a real error after a recovered warn=2 promotion in one statement", {
  # Under warn>=2 the warning is promoted to an error, which the statement's OWN
  # tryCatch catches and recovers from; the statement then raises a genuine,
  # uncaught error. That real error must still be recorded -- it must not be
  # masked by the earlier (recovered) promotion sharing the same stmt index.
  code <- c(
    "options(warn = 2)",
    "{ tryCatch(warning('promoted'), error = function(e) NULL); stop('real error') }"
  )
  res <- run_code(code, backend = "inprocess")
  msgs <- vapply(res$conditions, `[[`, character(1), "message")
  expect_true("real error" %in% msgs)
})

test_that("recovered promotions do not hide later errors sharing a message suffix", {
  code <- c(
    "options(warn = 2)",
    "{ tryCatch(warning('foo'), error = function(e) NULL); stop('not foo') }"
  )
  result <- run_code(code, backend = "inprocess")

  expect_identical(vapply(result$conditions, `[[`, character(1), "message"),
                   c("foo", "not foo"))
})

test_that("custom stops after recovered custom warning promotions remain recorded", {
  code <- c(
    "options(warn = 2)",
    "{ tryCatch(warning(structure(list(message = 'foo', call = NULL), class = c('myWarn', 'warning', 'condition'))), error = function(e) NULL); stop(structure(list(message = 'not foo', call = NULL), class = c('myStop', 'condition'))) }"
  )
  result <- run_code(code, backend = "inprocess")

  expect_identical(vapply(result$conditions, `[[`, character(1), "message"),
                   c("foo", "not foo"))
  expect_identical(result$conditions[[1L]]$classes, c("myWarn", "warning"))
  expect_identical(result$conditions[[2L]]$classes, "myStop")
})

test_that("empty and formatted promoted warning messages have no duplicate", {
  for (message in c("", "100% warning")) {
    result <- run_code(c("options(warn = 2)",
                         paste0("warning(", encodeString(message, quote = '"'), ")")),
                       backend = "inprocess")
    expect_length(result$conditions, 1L)
    expect_identical(result$conditions[[1L]]$message, message)
  }
})

test_that("truncated warning promotions retain their original custom classes", {
  for (limit in c(101L, 8170L)) {
    for (message in c(strrep("x", 10000L), strrep("é🙂漢", 2000L))) {
      code <- c(
        paste0("options(warn = 2, warning.length = ", limit, ")"),
        paste0("warning(structure(list(message = ",
               encodeString(message, quote = '"'),
               ", call = NULL), class = c('myWarn', 'warning', 'condition')))")
      )
      result <- run_code(code, backend = "inprocess")
      expect_length(result$conditions, 1L)
      expect_identical(result$conditions[[1L]]$message, message)
      expect_identical(result$conditions[[1L]]$classes, c("myWarn", "warning"))
      expect_identical(result$conditions[[1L]]$type, "error")
    }
  }
})

test_that("a genuine promotion-shaped error after recovery remains recorded", {
  code <- c(
    "options(warn = 2)",
    "{ tryCatch(warning('foo'), error = function(e) NULL); stop(sprintf(gettext('(converted from warning) %s', domain = 'R'), 'foo')) }"
  )
  result <- run_code(code, backend = "inprocess")

  expect_length(result$conditions, 2L)
  expect_identical(result$conditions[[2L]]$message,
                   sprintf(gettext("(converted from warning) %s", domain = "R"),
                           "foo"))
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

test_that("run_code captures uncaught conditions thrown by stop without error class", {
  code <- c(
    "cond <- structure(list(message = 'custom stop', call = NULL), class = c('myError', 'condition'))",
    "stop(cond)",
    "message('never runs')"
  )
  result <- run_code(code, backend = "inprocess")

  expect_true(result$observed)
  expect_length(result$conditions, 1L)
  expect_identical(result$conditions[[1L]]$type, "error")
  expect_identical(result$conditions[[1L]]$message, "custom stop")
  expect_identical(result$conditions[[1L]]$classes, "myError")
  expect_identical(result$conditions[[1L]]$stmt_index, 2L)
})

test_that("ordinary and caught custom conditions leave subsequent code running", {
  code <- c(
    "cond <- structure(list(message = 'custom signal', call = NULL), class = c('myCondition', 'condition'))",
    "signal <- signalCondition",
    "signal(cond)",
    "tryCatch(stop(cond), myCondition = function(c) NULL)",
    "stop('real error')"
  )
  result <- run_code(code, backend = "inprocess")

  expect_length(result$conditions, 1L)
  expect_identical(result$conditions[[1L]]$message, "real error")
  expect_identical(result$conditions[[1L]]$stmt_index, 5L)
})

test_that("warnings and messages in stop arguments retain normal handling", {
  code <- c(
    "cond <- structure(list(message = 'custom stop', call = NULL), class = c('myError', 'condition'))",
    "stop({ warning('argument warning'); message('argument message'); cond })"
  )
  result <- run_code(code, backend = "inprocess")

  expect_identical(vapply(result$conditions, `[[`, character(1), "type"),
                   c("warning", "message", "error"))
  expect_identical(vapply(result$conditions, `[[`, character(1), "message"),
                   c("argument warning", "argument message", "custom stop"))
})

test_that("specific-class reductions remain safe when condition classes are reduced", {
  script <- c(
    "n <- 3",
    "cond <- structure(class = c('myError', 'error', 'condition'), list(message = paste('value', n), call = NULL))",
    "stop(cond)"
  )
  result <- minex(code = script, match = "class", backend = "inprocess",
                  granularity = "expression")
  observed <- run_code(result$code, backend = "inprocess")

  expect_true(observed$observed)
  expect_true(build_matcher("class")(observed$conditions[[1L]], result$target))
  expect_identical(observed$conditions[[1L]]$type, "error")
})

test_that("callr captures malformed custom stops and specific-class reductions", {
  skip_on_cran()
  skip_if_not_installed("callr")
  script <- c(
    "n <- 3",
    "cond <- structure(class = c('myError', 'error', 'condition'), list(message = paste('value', n), call = NULL))",
    "stop(cond)"
  )
  malformed <- run_code(c(
    "cond <- structure(list(message = 'custom stop', call = NULL), class = c('myError', 'condition'))",
    "stop(cond)"
  ), backend = "callr")
  expect_true(malformed$observed)
  expect_identical(malformed$conditions[[1L]]$classes, "myError")
  result <- minex(code = script, match = "class", backend = "callr",
                  granularity = "expression")
  observed <- run_code(result$code, backend = "callr")
  expect_true(observed$observed)
  expect_true(build_matcher("class")(observed$conditions[[1L]], result$target))
})
