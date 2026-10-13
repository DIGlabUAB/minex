expect_actual_call_trace <- function(trace, calls) {
  expect_true(all(diff(trace$call) >= 0L))
  actual <- !trace$cached & trace$event == ""
  expect_equal(trace$call[actual], seq_len(calls))
}

test_that("public reducers reject malformed budgets before evaluating predicates", {
  invalid <- list(NA_real_, NaN, NULL, numeric(), c(1, 2), "3", -Inf, 0, -1, 1.5)
  for (budget in invalid) {
    predicate <- function(...) stop("must not evaluate")
    expect_error(ddmin(1:2, predicate, max_oracle_calls = budget), "max_oracle_calls")
    expect_error(reduce_rows(data.frame(x = integer()), predicate,
                             max_oracle_calls = budget), "max_oracle_calls")
    expect_error(minex(code = "f(a)", oracle = predicate,
                        max_oracle_calls = budget), "max_oracle_calls")
  }
})

test_that("ddmin counts the full predicate once within a tight budget", {
  for (algorithm in c("cdd", "ddmin")) {
    calls <- 0L
    info <- new.env()
    predicate <- function(items) { calls <<- calls + 1L; 2L %in% items }
    out <- suppressMessages(ddmin(1:4, predicate, algorithm = algorithm,
      max_oracle_calls = 1L, verbose = "trace", .info = info))
    expect_identical(out, 1:4)
    expect_identical(calls, 1L)
    expect_identical(info$oracle_calls, calls)
    expect_false(info$complete)
    expect_actual_call_trace(info$trace, calls)
  }
})

test_that("reduce_rows exposes incomplete results without a duplicate full predicate", {
  data <- data.frame(id = 1:4)
  seen <- list()
  predicate <- function(rows) {
    seen[[length(seen) + 1L]] <<- rows$id
    3L %in% rows$id
  }
  info <- new.env()
  expect_warning(out <- reduce_rows(data, predicate, max_oracle_calls = 1L,
                                    .info = info), "stopped early")
  expect_identical(out, data)
  expect_identical(info$oracle_calls, 1L)
  expect_length(seen, 1L)
  expect_false(info$complete)
  seen <- list()
  out <- reduce_rows(data, predicate, .info = info)
  expect_identical(out$id, 3L)
  expect_length(Filter(function(ids) identical(ids, data$id), seen), 1L)
  expect_identical(info$oracle_calls, length(seen))
  expect_true(info$complete)
  expect_null(attr(out, "complete"))
})

test_that("HDD's final empty-keep probe is budgeted and traced", {
  for (budget in 1:2) {
    calls <- 0L
    info <- new.env()
    out <- suppressMessages(hdd_star("f(a)", function(text) {
      calls <<- calls + 1L
      TRUE
    }, max_oracle_calls = budget, verbose = "trace", info = info))
    expect_identical(info$oracle_calls, calls)
    expect_identical(calls, budget)
    expect_identical(info$complete, budget == 2L)
    expect_identical(out, if (budget == 1L) "f(a)" else "f()")
    expect_true(any(info$trace$phase == "verify_empty"))
    expect_actual_call_trace(info$trace, calls)
  }
})

test_that("HDD cache hits do not charge the underlying oracle again", {
  seen <- character()
  oracle <- hdd_make_oracle(function(text) {
    seen <<- c(seen, text)
    grepl("bad", text, fixed = TRUE)
  })
  info <- new.env()
  out <- suppressMessages(hdd_star("f(a, bad, c)", oracle, verbose = "trace", info = info))
  expect_identical(parse(text = out)[[1]], quote(f(bad)))
  expect_identical(info$oracle_calls, length(seen))
  expect_false(anyDuplicated(seen) > 0L)
  expect_actual_call_trace(info$trace, length(seen))
})

test_that("HDD rejects invalid syntax without spending an oracle call", {
  # A future span collector could propose deleting a native pipe's callee.
  # Keep parsing real while injecting that unsafe span next to a valid argument.
  env <- new.env(parent = environment(hdd_star))
  env$hdd_reducible_by_depth <- function(text) {
    spans <- list(list(from = 6L, to = 6L, depth = 0L))
    if (identical(text, "x |> f(a)")) {
      spans[[2L]] <- list(from = 8L, to = 8L, depth = 0L)
    }
    list(spans)
  }
  reduce <- hdd_star
  environment(reduce) <- env
  for (budget in 1:2) {
    seen <- character()
    info <- new.env()
    out <- suppressMessages(reduce("x |> f(a)", function(text) {
      seen <<- c(seen, text)
      expect_true(pt_parses(text))
      TRUE
    }, max_oracle_calls = budget, verbose = "trace", info = info))
    expect_identical(info$oracle_calls, length(seen))
    expect_identical(length(seen), budget)
    expect_identical(info$complete, budget == 2L)
    expect_identical(out, if (budget == 1L) "x |> f(a)" else "x |> f()")
    expect_true(any(info$trace$event == "parse_rejected"))
    expect_actual_call_trace(info$trace, length(seen))
  }
})

test_that("source-set cache keys preserve statement vector boundaries", {
  calls <- 0L
  context <- new_oracle_context()
  oracle <- oracle_wrap(function(code) { calls <<- calls + 1L; length(code) == 2L }, context)
  expect_true(oracle(c("a", "b")))
  expect_false(oracle("a\nb"))
  expect_true(oracle(c("a", "b")))
  expect_identical(calls, 2L)
})

test_that("joint verification and post-HDD sweeps share actual-call accounting", {
  original <- c("a <- 1", "f(a, bad)")
  reproduces <- function(code) {
    any(grepl("bad", code, fixed = TRUE)) &&
      (!any(grepl("a,", code, fixed = TRUE)) || length(code) == 2L)
  }
  for (budget in c(1:12, Inf)) {
    seen <- character()
    oracle <- function(code) {
      seen <<- c(seen, oracle_key(code))
      reproduces(code)
    }
    result <- suppressWarnings(suppressMessages(minex(code = original, oracle = oracle,
      granularity = "expression", max_oracle_calls = budget, verbose = "trace")))
    expect_identical(result$oracle_calls, length(seen))
    expect_lte(length(seen), budget)
    expect_false(anyDuplicated(seen) > 0L)
    expect_true(reproduces(result$code))
    expect_actual_call_trace(result$trace, length(seen))
    if (result$complete) expect_identical(parse(text = result$code)[[1]], quote(f(bad)))
    if (!is.finite(budget)) {
      expect_true(any(result$trace$phase == "joint_verify"))
      expect_true(any(result$trace$phase == "post_hdd_verify"))
    }
  }
})

test_that("jointly invalid expression reductions fall back within the same budget", {
  original <- c("z1 <- 1 |> abs() |> sqrt()", "z2 <- 2 |> abs() |> sqrt()")
  reproduces <- function(code) {
    length(code) == 2L && !all(nchar(code) < nchar(original))
  }
  seen <- character()
  oracle <- function(code) { seen <<- c(seen, oracle_key(code)); reproduces(code) }
  expect_warning(result <- suppressMessages(minex(code = original, oracle = oracle,
    granularity = "expression", verbose = "trace")), "did not jointly reproduce")
  expect_identical(result$code, original)
  expect_false(result$complete)
  expect_identical(result$oracle_calls, length(seen))
  expect_false(anyDuplicated(seen) > 0L)
  expect_actual_call_trace(result$trace, length(seen))
})

test_that("automatic escalation shares custom preconditions, budgets, and trace IDs", {
  original <- c("x <- 1", "f(x, bad, unused)")
  seen <- character()
  oracle <- function(code) {
    seen <<- c(seen, oracle_key(code))
    length(code) == 2L && any(grepl("bad", code, fixed = TRUE))
  }
  result <- suppressMessages(minex(code = original, oracle = oracle, verbose = "trace"))
  expect_identical(result$escalated_from, "statement")
  expect_identical(result$oracle_calls, length(seen))
  expect_false(anyDuplicated(seen) > 0L)
  expect_equal(sum(seen == oracle_key(original)), 1L)
  expect_actual_call_trace(result$trace, length(seen))
})

test_that("a non-reflexive matcher is not assumed to confirm the full source", {
  expect_error(minex(code = "stop('boom')", backend = "inprocess",
    match = function(candidate, target) FALSE, granularity = "statement"),
    "nothing to minimize")
})
