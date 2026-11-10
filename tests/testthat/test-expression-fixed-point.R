test_that("later expression edits unlock earlier statements across passes", {
  original <- c("f(a,b)", "g(c,d)")
  reproduces <- function(code) {
    length(code) == 2L && grepl("b", code[1]) && grepl("d", code[2]) &&
      (grepl("a", code[1]) || !grepl("c", code[2]))
  }
  for (algorithm in c("cdd", "ddmin")) {
    result <- minex(code = original, oracle = reproduces, algorithm = algorithm,
                    granularity = "expression")
    expect_identical(result$code, c("f(b)", "g(d)"))
    expect_true(result$complete)
    # Default granularity must converge through the same automatic escalation.
    escalated <- minex(code = original, oracle = reproduces, algorithm = algorithm)
    expect_identical(escalated$code, result$code)
    expect_identical(escalated$escalated_from, "statement")
    expect_true(escalated$complete)
  }
})

test_that("expression convergence is not limited to one additional pass", {
  reproduces <- function(code) {
    length(code) == 3L && grepl("b", code[1]) && grepl("d", code[2]) &&
      grepl("z", code[3]) && (grepl("a", code[1]) || !grepl("c", code[2])) &&
      (grepl("c", code[2]) || !grepl("e", code[3]))
  }
  result <- minex(code = c("f(a,b)", "g(c,d)", "h(e,z)"), oracle = reproduces,
                  granularity = "expression")
  expect_identical(result$code, c("f(b)", "g(d)", "h(z)"))
  expect_true(result$complete)
})

test_that("a post-expression statement deletion unlocks an earlier expression", {
  reproduces <- function(code) {
    if (length(code) < 2L || !grepl("b", code[1]) || !grepl("d", code[2])) {
      return(FALSE)
    }
    marker <- any(code == "marker <- TRUE")
    (marker || !grepl("c", code[2])) && (grepl("a", code[1]) || !marker)
  }
  result <- minex(code = c("f(a,b)", "g(c,d)", "marker <- TRUE"),
                  oracle = reproduces, granularity = "expression")
  expect_identical(result$code, c("f(b)", "g(d)"))
  expect_true(result$complete)
})

test_that("budget exhaustion during a later expression pass retains confirmed progress", {
  reproduces <- function(code) {
    length(code) == 2L && grepl("b", code[1]) && grepl("d", code[2]) &&
      (grepl("a", code[1]) || !grepl("c", code[2]))
  }
  for (algorithm in c("cdd", "ddmin")) {
    run <- function(budget) {
      seen <- character()
      oracle <- function(code) {
        seen <<- c(seen, oracle_key(code))
        reproduces(code)
      }
      result <- suppressWarnings(suppressMessages(minex(
        code = c("f(a,b)", "g(c,d)"), oracle = oracle,
        granularity = "expression", algorithm = algorithm,
        max_oracle_calls = budget, verbose = "trace"
      )))
      expect_true(reproduces(result$code))
      expect_identical(result$oracle_calls, length(seen))
      expect_lte(length(seen), budget)
      expect_equal(anyDuplicated(seen), 0L)
      actual <- !result$trace$cached & result$trace$event == ""
      expect_identical(result$trace$call[actual], seq_along(seen))
      result
    }
    full <- run(Inf)
    expect_true(full$complete)
    partial <- lapply(seq_len(full$oracle_calls), run)
    expect_true(any(vapply(partial, function(result) {
      !result$complete && identical(result$code, c("f(a,b)", "g(d)"))
    }, logical(1))))
    expect_true(any(vapply(partial, function(result) {
      !result$complete && identical(result$code, full$code)
    }, logical(1))))
    expect_true(partial[[length(partial)]]$complete)
    expect_identical(partial[[length(partial)]]$code, full$code)
  }
})

test_that("expression fixed points satisfy every three-deletion predicate", {
  # Enumerate every predicate over two positional arguments and a removable
  # statement, with the original configuration true. The b/d arguments and
  # their enclosing statements stay required. This covers non-monotone
  # interactions between expression edits and whole-statement deletions.
  original <- c("f(a,b)", "g(c,d)", "marker <- TRUE")
  for (mask in 0:127) {
    outcomes <- c(as.logical(intToBits(mask)[1:7]), TRUE)
    predicate <- function(code) {
      if (length(code) < 2L || !grepl("b", code[1]) || !grepl("d", code[2])) {
        return(FALSE)
      }
      key <- 1L + grepl("a", code[1]) + 2L * grepl("c", code[2]) +
        4L * any(code == "marker <- TRUE")
      outcomes[key]
    }
    for (algorithm in c("cdd", "ddmin")) {
      for (budget in c(1, 5, Inf)) {
        seen <- character()
        oracle <- function(code) {
          seen <<- c(seen, oracle_key(code))
          predicate(code)
        }
        result <- suppressWarnings(minex(
          code = original, oracle = oracle, algorithm = algorithm,
          granularity = "expression", max_oracle_calls = budget
        ))
        context <- paste("predicate", mask, "algorithm", algorithm, "budget", budget)
        expect_true(predicate(result$code), info = context)
        expect_true(all(vapply(result$code, pt_parses, logical(1))), info = context)
        expect_identical(result$oracle_calls, length(seen), info = context)
        expect_true(length(seen) <= budget, info = context)
        expect_equal(anyDuplicated(seen), 0L, info = context)
        if (isTRUE(result$complete)) {
          for (i in seq_along(result$code)) {
            expect_false(predicate(result$code[-i]), info = context)
            for (level in hdd_reducible_by_depth(result$code[i])) {
              for (span in level) {
                candidate <- result$code
                candidate[i] <- pt_delete(candidate[i], list(span))
                if (pt_parses(candidate[i])) expect_false(predicate(candidate), info = context)
              }
            }
          }
        } else {
          expect_true(is.finite(budget), info = context)
        }
      }
    }
  }
})
