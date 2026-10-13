test_that("shared search budgets preserve accounting, caching, and minimality", {
  # A real custom oracle sees whole source configurations across both passes.
  # Statement reduction discards g(); expression reduction can discard aa/cc.
  code <- c("f(aa, bb, cc)", "g(dd, ee)")
  predicate <- function(statements) any(grepl("bb", statements, fixed = TRUE))
  cases <- expand.grid(algorithm = c("cdd", "ddmin"),
                       granularity = c("statement", "expression"),
                       budget = c(1:20, Inf), stringsAsFactors = FALSE)

  for (case in seq_len(nrow(cases))) {
    calls <- 0L
    configurations <- list()
    counted <- function(statements) {
      calls <<- calls + 1L
      configurations[[calls]] <<- serialize(statements, NULL)
      predicate(statements)
    }
    context <- paste("algorithm", cases$algorithm[[case]],
                     "granularity", cases$granularity[[case]],
                     "budget", cases$budget[[case]])
    result <- suppressMessages(suppressWarnings(minex(
      code = code, oracle = counted, backend = "inprocess",
      algorithm = cases$algorithm[[case]],
      granularity = cases$granularity[[case]],
      max_oracle_calls = cases$budget[[case]], verbose = "trace"
    )))

    expect_true(predicate(result$code), info = context)
    expect_identical(result$oracle_calls, calls, info = context)
    expect_true(calls <= cases$budget[[case]], info = context)
    expect_equal(anyDuplicated(configurations), 0L, info = context)
    trace <- result$trace
    real_calls <- !trace$cached & trace$event == ""
    expect_equal(sum(real_calls), calls, info = context)
    expect_identical(trace$call[real_calls], seq_len(calls), info = context)
    expect_true(all(diff(trace$call) >= 0L), info = context)
    expect_true(all(trace$call >= 1L & trace$call <= calls), info = context)

    if (isTRUE(result$complete)) {
      for (i in seq_along(result$code)) {
        expect_false(predicate(result$code[-i]), info = context)
        if (cases$granularity[[case]] == "expression") {
          # Check every supported span against the final whole script, rather
          # than trusting the reducer's own completion flag or cached outcome.
          for (level in hdd_reducible_by_depth(result$code[[i]])) {
            for (span in level) {
              candidate <- result$code
              candidate[[i]] <- pt_delete(candidate[[i]], list(span))
              if (pt_parses(candidate[[i]])) {
                expect_false(predicate(candidate), info = context)
              }
            }
          }
        }
      }
    } else {
      expect_true(is.finite(cases$budget[[case]]), info = context)
    }
  }
})
