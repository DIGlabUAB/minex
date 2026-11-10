test_that("inprocess evaluations restore added, removed and changed options", {
  withr::local_options(list(
    minex.test.option.added = NULL,
    minex.test.option.existing = "original"
  ))
  before <- options()

  endings <- c(
    "invisible(NULL)",
    "stop('evaluation failed')",
    "tryCatch(warning('caught'), warning = function(w) NULL)"
  )
  for (ending in endings) {
    result <- run_code(c(
      "options(minex.test.option.added = TRUE, minex.test.option.existing = NULL, warn = 2)",
      ending
    ), backend = "inprocess")

    expect_true(result$observed)
    expect_setequal(names(options()), names(before))
    expect_identical(options()[names(before)], before)
    expect_null(getOption("minex.test.option.added"))
  }
})

test_that("new options cannot affect a later independent evaluation", {
  withr::local_options(list(minex.test.option.added = NULL))

  first <- run_code(c(
    "options(minex.test.option.added = TRUE)",
    "stop('first failure')"
  ), backend = "inprocess")
  second <- run_code(
    "if (isTRUE(getOption('minex.test.option.added'))) stop('leaked option')",
    backend = "inprocess"
  )

  expect_identical(first$conditions[[1]]$message, "first failure")
  expect_true(second$observed)
  expect_length(second$conditions, 0L)
})

test_that("minex retains setup required to reproduce an option-dependent error", {
  withr::local_options(list(minex.test.option.required = NULL))
  code <- c(
    "options(minex.test.option.required = TRUE)",
    "if (isTRUE(getOption('minex.test.option.required'))) stop('option-dependent failure')"
  )

  result <- minex(code = code, backend = "inprocess",
                  granularity = "statement")

  expect_length(result$code, 2L)
  expect_null(getOption("minex.test.option.required"))
  reproduced <- run_code(result$code, backend = "inprocess")
  expect_identical(reproduced$conditions[[1]]$message,
                   "option-dependent failure")
  without_setup <- run_code(result$code[-1L], backend = "inprocess")
  expect_length(without_setup$conditions, 0L)
})
