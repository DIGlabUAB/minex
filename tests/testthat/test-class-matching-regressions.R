test_that("class reduction keeps the setup required for a specific failure", {
  code <- c(
    "zz_minex_class_x <- 1",
    paste0("if (zz_minex_class_x == 1) stop(structure(",
           "list(message = 'intended', call = NULL), ",
           "class = c('DomainError', 'error', 'condition')))"))

  cases <- expand.grid(mode = c("class", "both"),
                       granularity = c("statement", "expression"),
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    result <- minex(code = code, match = cases$mode[[i]], backend = "inprocess",
                    granularity = cases$granularity[[i]])
    observed <- run_code(result$code, backend = "inprocess")
    reduced <- pick_target_condition(observed$conditions, "error")
    expect_true(observed$observed)
    expect_false(is.null(reduced))
    expect_contains(reduced$classes, "DomainError")
    expect_identical(reduced$message, "intended")
    if (cases$granularity[[i]] == "statement") expect_equal(result$n_minimal, 2L)
  }
})

test_that("specific class reduction can change the message without changing the cause", {
  code <- c(
    "zz_minex_class_message <- 'original'",
    paste0("stop(structure(list(message = if (exists('zz_minex_class_message', ",
           "inherits = FALSE)) zz_minex_class_message else 'reduced', call = NULL), ",
           "class = c('DomainError', 'error', 'condition')))"))

  result <- minex(code = code, match = "class", backend = "inprocess",
                  granularity = "statement")
  reduced <- pick_target_condition(
    run_code(result$code, backend = "inprocess")$conditions, "error")
  expect_equal(result$n_minimal, 1L)
  expect_contains(reduced$classes, "DomainError")
  expect_identical(reduced$message, "reduced")

  both <- minex(code = code, match = "both", backend = "inprocess",
                granularity = "statement")
  both_condition <- pick_target_condition(
    run_code(both$code, backend = "inprocess")$conditions, "error")
  expect_equal(both$n_minimal, 2L)
  expect_identical(both_condition$message, "original")
  expect_contains(both_condition$classes, "DomainError")
})

test_that("generic class reduction preserves the failure message", {
  code <- c("zz_minex_class_x <- 1",
            "if (zz_minex_class_x == 1) stop('intended generic error')")
  result <- minex(code = code, match = "class", backend = "inprocess",
                  granularity = "statement")
  reduced <- pick_target_condition(
    run_code(result$code, backend = "inprocess")$conditions, "error")
  expect_equal(result$n_minimal, 2L)
  expect_identical(reduced$message, "intended generic error")
  expect_contains(reduced$classes, "simpleError")
})
