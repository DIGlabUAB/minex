test_that("both reducers satisfy invariants for every three-item predicate", {
  # Enumerate all predicates whose empty set is false and full set is true.
  # This includes non-monotone cases where a deletion unlocks another one.
  items <- 1:3
  subset_key <- function(x) sum(2^(x - 1L)) + 1L
  for (mask in 0:63) {
    outcomes <- c(FALSE, as.logical(intToBits(mask)[1:6]), TRUE)
    predicate <- function(x) outcomes[subset_key(x)]
    for (algorithm in c("cdd", "ddmin")) {
      for (budget in c(1, 2, Inf)) {
        calls <- 0L
        configurations <- character()
        counted <- function(x) {
          calls <<- calls + 1L
          configurations <<- c(configurations, paste(x, collapse = ","))
          predicate(x)
        }
        info <- new.env(parent = emptyenv())
        result <- ddmin(items, counted, algorithm = algorithm,
                        max_oracle_calls = budget, .info = info)
        context <- paste("predicate", mask, "algorithm", algorithm, "budget", budget)
        expect_true(predicate(result), info = context)
        expect_identical(result, items[items %in% result], info = context)
        expect_equal(info$oracle_calls, calls, info = context)
        expect_true(calls <= budget, info = context)
        expect_equal(anyDuplicated(configurations), 0L, info = context)
        if (isTRUE(info$complete)) {
          removable <- vapply(seq_along(result), function(i) predicate(result[-i]),
                              logical(1))
          expect_false(any(removable), info = context)
        } else {
          expect_true(is.finite(budget), info = context)
        }
      }
    }
  }
})
