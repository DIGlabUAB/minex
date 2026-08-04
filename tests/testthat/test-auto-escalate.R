# A failure nested inside a function body: both top-level statements are
# required, so statement-level bisection can remove nothing, but the body has
# removable sub-expressions. This is the dominant shape of LLM-written R.
nested_failure <- c(
  "f <- function(x) {",
  "  y <- x + 1",
  "  z <- nonexistent_fn(y)",
  "  z * 2",
  "}",
  "f(1)"
)

test_that("minex escalates to expression granularity when statements cannot reduce", {
  res <- minex(code = nested_failure, backend = "inprocess")
  expect_equal(res$granularity, "expression")
  expect_identical(res$escalated_from, "statement")
  expect_lt(res$n_chars_minimal, res$n_chars_original)
})

test_that("escalation records the cost of the discarded coarse pass", {
  res <- minex(code = nested_failure, backend = "inprocess")
  expect_true(is.numeric(res$coarse_oracle_calls))
  expect_gt(res$coarse_oracle_calls, 0)
})

test_that("an explicitly requested granularity is never overridden", {
  res <- minex(code = nested_failure, backend = "inprocess",
               granularity = "statement")
  expect_equal(res$granularity, "statement")
  expect_null(res$escalated_from)
  expect_equal(res$n_minimal, res$n_original)
})

test_that("no escalation when statement bisection already reduces", {
  res <- minex(code = c("a <- 1", "b <- 2", "stop('boom')"),
               backend = "inprocess")
  expect_equal(res$granularity, "statement")
  expect_null(res$escalated_from)
  expect_lt(res$n_minimal, res$n_original)
})

test_that("the escalated result still reproduces the original failure", {
  res <- minex(code = nested_failure, backend = "inprocess")
  expect_match(res$target$message, "nonexistent_fn")
  expect_match(as.character(res), "nonexistent_fn")
})

# `max_oracle_calls` bounds the whole call, so the escalated pass must run on
# what the coarse pass left rather than on a fresh budget. Without this the
# bound is silently doubled.
test_that("escalation spends the remaining budget, not a fresh one", {
  # Unbudgeted, this script costs 3 coarse calls plus 10 fine ones.
  budget <- 12
  res <- suppressWarnings(
    minex(code = nested_failure, backend = "inprocess",
          max_oracle_calls = budget))
  expect_identical(res$escalated_from, "statement")
  expect_lte(res$oracle_calls, budget)
})

# A coarse pass that removed nothing because it ran out of calls has not
# established that nothing is removable, so it is not grounds to escalate.
test_that("no escalation when the coarse pass exhausted its budget", {
  res <- suppressWarnings(
    minex(code = nested_failure, backend = "inprocess", max_oracle_calls = 2))
  expect_equal(res$granularity, "statement")
  expect_null(res$escalated_from)
  expect_false(res$complete)
})

test_that("oracle_calls on an escalated result counts both passes", {
  res <- minex(code = nested_failure, backend = "inprocess")
  # Derive the expectation from the two passes the escalation is made of,
  # rather than from a literal, so this pins the contract and not one script.
  statement_only <- minex(code = nested_failure, backend = "inprocess",
                          granularity = "statement")
  expression_only <- minex(code = nested_failure, backend = "inprocess",
                           granularity = "expression")
  expect_equal(res$coarse_oracle_calls, statement_only$oracle_calls)
  # The discarded coarse pass was still work the caller paid for.
  expect_equal(res$oracle_calls,
               statement_only$oracle_calls + expression_only$oracle_calls)
})
