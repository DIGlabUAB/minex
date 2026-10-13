evaluation_failure_fixture <- function(type = "timeout") {
  cause <- structure(
    list(message = "controlled backend failure", call = NULL),
    class = c("controlled_backend_error", "error", "condition")
  )
  list(observed = FALSE, conditions = list(), failure = list(
    type = type, message = conditionMessage(cause), classes = class(cause),
    cause = cause
  ))
}

evaluation_mock_minex <- function(mock_run_code) {
  env <- new.env(parent = environment(minex))
  env$run_code <- mock_run_code
  minimize <- minex
  environment(minimize) <- env
  env$minex <- minimize
  minimize
}

test_that("an unobserved input preserves backend cause in a classed error", {
  failed <- evaluation_failure_fixture()
  minimize <- evaluation_mock_minex(function(...) failed)

  error <- tryCatch(
    minimize(code = "stop('boom')", backend = "inprocess"),
    minex_evaluation_error = identity
  )

  expect_s3_class(error, "minex_evaluation_error")
  expect_match(conditionMessage(error), "input evaluation could not be observed")
  expect_match(conditionMessage(error), "controlled backend failure")
  expect_identical(error$parent, failed$failure$cause)
  expect_identical(error$failure, failed$failure)
  expect_identical(error$backend, "inprocess")
})

test_that("an observed baseline is reused without a second backend evaluation", {
  real_run_code <- run_code
  runs <- 0L
  failed <- evaluation_failure_fixture("process_failure")
  minimize <- evaluation_mock_minex(function(...) {
    runs <<- runs + 1L
    if (runs == 1L) real_run_code(...) else failed
  })

  result <- minimize(code = "stop('boom')", backend = "inprocess",
                     granularity = "statement")
  expect_true(result$complete)
  expect_length(result$evaluation_failures, 0L)
  expect_identical(runs, 1L)
  expect_identical(result$oracle_calls, 0L)
})

test_that("unobserved candidate deletions retain progress and mark incomplete", {
  real_run_code <- run_code
  failed <- evaluation_failure_fixture()
  minimize <- evaluation_mock_minex(function(code, ...) {
    if (identical(code, "stop('boom')")) return(failed)
    real_run_code(code, ...)
  })

  expect_warning(
    result <- minimize(code = c("setup <- 1", "stop('boom')"),
                    backend = "inprocess", granularity = "statement"),
    "candidate evaluation.*could not be observed"
  )

  expect_identical(result$code, c("setup <- 1", "stop('boom')"))
  expect_false(result$complete)
  expect_length(result$evaluation_failures, 1L)
  expect_identical(result$evaluation_failures[[1L]]$code, "stop('boom')")
  expect_identical(result$evaluation_failures[[1L]]$failure, failed$failure)
  expect_null(result$escalated_from)
  observed <- real_run_code(result$code, backend = "inprocess")
  expect_identical(observed$conditions[[1L]]$message, "boom")
  output <- capture.output(print(result))
  expect_true(any(grepl("could not be observed", output)))
  expect_false(any(grepl("higher.*max_oracle_calls", output)))
})

test_that("an unobserved truncation probe retains the original confirmed code", {
  real_run_code <- run_code
  failed <- evaluation_failure_fixture()
  minimize <- evaluation_mock_minex(function(code, ...) {
    if (length(code) < 3L) return(failed)
    real_run_code(code, ...)
  })

  expect_warning(
    result <- minimize(code = c("setup <- 1", "stop('boom')", "tail <- 2"),
                    backend = "inprocess", granularity = "statement"),
    "candidate evaluation.*could not be observed"
  )
  expect_length(result$code, 3L)
  expect_false(result$complete)
  expect_gt(length(result$evaluation_failures), 0L)
})

test_that("callr timeout is unobserved and retains its backend condition", {
  skip_on_cran()
  skip_if_not_installed("callr")

  result <- run_code("Sys.sleep(3)", backend = "callr", timeout = 0.2)

  expect_false(result$observed)
  expect_length(result$conditions, 0L)
  expect_identical(result$failure$type, "timeout")
  expect_s3_class(result$failure$cause, "callr_timeout_error")
  expect_identical(result$failure$message,
                   conditionMessage(result$failure$cause))
  expect_identical(result$failure$classes, class(result$failure$cause))
  expect_error(minex(code = "Sys.sleep(3)", backend = "callr", timeout = 0.2),
               class = "minex_evaluation_error")
})

test_that("callr process exit is distinguished from a clean observed run", {
  skip_on_cran()
  skip_if_not_installed("callr")

  result <- run_code("q(save = 'no', status = 1)", backend = "callr", timeout = 5)

  expect_false(result$observed)
  expect_length(result$conditions, 0L)
  expect_identical(result$failure$type, "process_failure")
  expect_s3_class(result$failure$cause, "callr_status_error")
  expect_identical(result$failure$message,
                   conditionMessage(result$failure$cause))
  clean <- run_code("invisible(NULL)", backend = "callr")
  expect_true(clean$observed)
  expect_length(clean$conditions, 0L)
  expect_null(clean$failure)
})

test_that("callr candidate timeouts retain setup and report incomplete progress", {
  skip_on_cran()
  skip_if_not_installed("callr")
  script <- c(
    "ready <- TRUE",
    "if (exists('ready')) stop('boom') else Sys.sleep(3)"
  )

  expect_warning(
    result <- minex(code = script, backend = "callr", timeout = 1,
                    granularity = "statement"),
    "candidate evaluation.*could not be observed"
  )

  expect_false(result$complete)
  expect_identical(result$code, script)
  expect_length(result$evaluation_failures, 1L)
  expect_identical(result$evaluation_failures[[1L]]$failure$type, "timeout")
  expect_s3_class(result$evaluation_failures[[1L]]$failure$cause,
                  "callr_timeout_error")
  observed <- run_code(result$code, backend = "callr", timeout = 5)
  expect_true(observed$observed)
  expect_identical(observed$conditions[[1L]]$message, "boom")
})
