test_that("minex strips statements irrelevant to the failure", {
  script <- c(
    "a <- 10",
    "b <- 20",
    "log('not a number')"
  )
  res <- minex(code = script, backend = "inprocess")
  expect_s3_class(res, "minex_result")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "log\\('not a number'\\)")
})

test_that("minex keeps statements the failure depends on", {
  script <- c(
    "x <- c(1, 2, NA)",
    "m <- mean(x)",
    "if (is.na(m)) stop('mean is NA')"
  )
  res <- minex(code = script, backend = "inprocess")
  # All three lines are required to reproduce the error.
  expect_equal(res$n_minimal, 3L)
})

test_that("minex errors when the input does not fail", {
  expect_error(
    minex(code = c("x <- 1", "y <- 2"), backend = "inprocess"),
    "nothing to minimize"
  )
})

test_that("minex requires either file or code", {
  expect_error(minex(), "Supply `code`")
})

test_that("minex re-sweeps statements made redundant by sub-expression reduction", {
  # HDD drops the `zz_dat` argument from transform(), so the failure stops
  # depending on the data-frame statement. That statement must then be swept out
  # for the expression-granularity result to stay statement-minimal.
  code <- c("zz_dat <- data.frame(qq = 1:3)",
            "res <- transform(zz_dat, bb = zz_missing_col + qq)")
  res <- minex(code = code, backend = "inprocess", granularity = "expression")
  expect_equal(res$n_minimal, 1L)
  expect_match(res$code, "transform")
  expect_false(any(grepl("zz_dat <-", res$code)))
})

test_that("minex counts the failure-point truncation probe in oracle_calls", {
  # Failure at statement 1 of 2 -> truncation to [1] fires exactly one real oracle
  # probe. The reported oracle_calls must include it: 1 probe + 1 ddmin precondition
  # (a one-statement set needs no further reduction). The one-time target-recording
  # run is not a predicate evaluation and is not counted.
  res <- minex(code = c("stop('boom')", "y <- 1"), backend = "inprocess")
  expect_equal(res$n_minimal, 1L)
  expect_equal(res$oracle_calls, 2L)
})

test_that("minex reads from a file", {
  path <- tempfile(fileext = ".R")
  writeLines(c("ok <- TRUE", "stop('from file')"), path)
  on.exit(unlink(path), add = TRUE)
  res <- minex(file = path, backend = "inprocess")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "from file")
})

test_that("minex accepts a custom oracle", {
  script <- c("one <- 1", "two <- 2", "three <- 3")
  # Reproduce the "failure" of containing the assignment to `two`.
  res <- minex(
    code = script,
    oracle = function(stmts) any(grepl("two", stmts)),
    backend = "inprocess"
  )
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "two")
  expect_null(res$target)
})

test_that("minex validates a non-function oracle", {
  expect_error(
    minex(code = "stop('x')", oracle = 42, backend = "inprocess"),
    "must be a function"
  )
})

test_that("class matching tolerates a changed message", {
  # A custom-classed condition; the message varies but the class is stable.
  script <- c(
    "n <- 3",
    "cond <- structure(class = c('myError', 'error', 'condition'),",
    "                  list(message = paste('value', n), call = NULL))",
    "stop(cond)"
  )
  res <- minex(code = script, match = "class", backend = "inprocess")
  expect_true(res$n_minimal >= 1L)
})

test_that("print returns its input invisibly", {
  res <- minex(code = "stop('boom')", backend = "inprocess")
  expect_output(print(res), "minex_result")
  expect_invisible(print(res))
})

test_that("callr backend reproduces the in-process result", {
  skip_on_cran()
  skip_if_not_installed("callr")
  script <- c("a <- 1", "b <- 2", "stop('callr boom')")
  res <- minex(code = script, backend = "callr")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "callr boom")
})

test_that("minex targets a warning with condition = 'warning'", {
  script <- c("x <- 1", "as.numeric('a')")  # produces a warning (NA coercion)
  res <- minex(code = script, condition = "warning", backend = "inprocess")
  expect_s3_class(res, "minex_result")
  expect_true(res$n_minimal >= 1L)
})

test_that("minex errors when the requested condition type never occurs", {
  expect_error(
    minex(code = "stop('boom')", condition = "warning", backend = "inprocess"),
    "warning"
  )
})

test_that("minex warns on multiple input sources (code wins, resolves cleanly)", {
  # Use code + file so `code` wins and resolves WITHOUT touching the clipboard
  # (which would stop() on headless CI and turn the warning test into an error).
  path <- tempfile(fileext = ".R"); writeLines("y <- 2", path)
  on.exit(unlink(path), add = TRUE)
  expect_warning(
    minex(code = "stop('x')", file = path, backend = "inprocess"),
    "Multiple input|code > clipboard"
  )
})

test_that("minex reports incomplete under a tight budget", {
  # Jointly-required, non-adjacent statements force several oracle calls, so a
  # budget of 1 genuinely truncates the search.
  script <- c("a <- 1", "b <- 2", "c <- 3", "stop('boom')")
  res <- suppressWarnings(
    minex(code = script, backend = "inprocess", max_oracle_calls = 1L)
  )
  expect_false(res$complete)
})

test_that("minex truncates statements after the failing one", {
  script <- c("a <- 1", "stop('boom')", "b <- 2", "c <- 3")
  res <- minex(code = script, backend = "inprocess")
  # The trailing statements are gone: minimal result is just the failing line.
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "boom")
  expect_false(grepl("c <- 3", as.character(res)))
})

test_that("minex reduces against the promoted error under warn=2", {
  script <- c("options(warn = 2)", "warning('w')", "y <- 1")
  res <- minex(code = script, backend = "inprocess")
  expect_match(as.character(res), "warning")
})

test_that("minex accepts a function for `match` (custom condition matching)", {
  # Regression guard: `match.arg(match)` must not run on a function argument.
  script <- c("setup <- 1", "stop('object xyz not found')")
  res <- minex(
    code = script,
    match = function(candidate, target) grepl("not found", candidate$message),
    backend = "inprocess"
  )
  expect_s3_class(res, "minex_result")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "not found")
})

test_that("granularity='expression' reduces inside a pipeline", {
  script <- c("x <- 1:10",
              "result <- x |> rev() |> sqrt() |> log('oops not a base')")
  res <- minex(code = script, granularity = "expression", backend = "inprocess")
  expect_s3_class(res, "minex_result")
  expect_true(grepl("log", as.character(res), fixed = TRUE))
  expect_false(grepl("rev(", as.character(res), fixed = TRUE))
})

test_that("granularity='statement' (default) keeps statement-level behaviour", {
  script <- c("a <- 1", "b <- 2", "log('not a number')")
  res <- minex(code = script, backend = "inprocess")
  expect_equal(res$n_minimal, 1L)
  expect_match(as.character(res), "log")
  # Additive result fields must not perturb the statement-granularity path.
  expect_equal(res$granularity, "statement")
  expect_type(res$n_chars_original, "integer")
  expect_type(res$n_chars_minimal, "integer")
  expect_type(res$complete, "logical")
  expect_true(res$complete)
  expect_type(res$oracle_calls, "integer")
})

test_that("minex_result keeps n_minimal integer and code a per-statement vector", {
  script <- c("x <- 1:3", "result <- x |> rev() |> log('oops')")
  res <- minex(code = script, granularity = "expression", backend = "inprocess")
  expect_type(res$n_minimal, "integer")
  expect_equal(res$n_minimal, length(res$code))
  expect_type(res$n_chars_minimal, "integer")
})

test_that("granularity='expression' pipeline reduction produces the exact expected code", {
  script <- c("x <- 1:10",
              "result <- x |> rev() |> sqrt() |> log('oops not a base')")
  res <- minex(code = script, granularity = "expression", backend = "inprocess")
  expect_equal(
    res$code,
    c("x <- 1:10", "result <- x |> log('oops not a base')")
  )
})

test_that("granularity='expression' result always reproduces the target failure (joint-soundness)", {
  # Guards against the joint-soundness gap: each statement is reduced against
  # the OTHER statements held at their un-reduced form, so the assembled
  # code_out must be joint-verified (or fall back) before being returned.
  script <- c("x <- 1:10",
              "result <- x |> rev() |> sqrt() |> log('oops not a base')")
  res <- minex(code = script, granularity = "expression", backend = "inprocess")

  rerun <- run_code(res$code, backend = "inprocess")
  cand <- pick_target_condition(rerun$conditions, res$condition)
  expect_false(is.null(cand))
  matcher <- build_matcher(res$match)
  expect_true(isTRUE(matcher(cand, res$target)))
})

test_that("granularity='expression' falls back to the statement-level result when sub-expression reductions don't jointly reproduce", {
  # A custom oracle that deterministically returns TRUE for any candidate in
  # which at most one of the two statements is shorter than its original
  # (i.e. reproduces per-statement, which is all the inner HDD loop checks,
  # since the loop always holds the OTHER statement at its unreduced form)
  # but FALSE once BOTH statements are simultaneously shortened. This
  # reliably forces the two per-statement reductions to independently
  # "succeed" while their composition does not -- the exact defect class the
  # final joint-verify + fallback exists to catch.
  orig <- c("z1 <- 1 |> abs() |> sqrt()",
            "z2 <- 2 |> abs() |> sqrt()")
  fallback_oracle <- function(stmts) {
    if (length(stmts) != 2) return(FALSE)
    reduced <- c(nchar(stmts[1]) < nchar(orig[1]),
                 nchar(stmts[2]) < nchar(orig[2]))
    !(reduced[1] && reduced[2])
  }

  expect_warning(
    res <- minex(code = orig, oracle = fallback_oracle,
                 granularity = "expression", backend = "inprocess"),
    "did not jointly reproduce"
  )
  expect_identical(res$code, orig)   # fell back to the statement-level form
  expect_false(res$complete)
})

test_that("granularity='expression' warns when the HDD phase (not statement-level ddmin) hits budget", {
  # budget=6 lets statement-level ddmin finish (it needs 3 calls here and
  # both statements are jointly required) but truncates the subsequent HDD
  # pass, so this exercises the budget-limited path distinct from the
  # non-composition fallback above.
  script <- c("x <- 1:10",
              "result <- x |> rev() |> sqrt() |> log('oops not a base')")
  res <- suppressWarnings(
    minex(code = script, granularity = "expression", backend = "inprocess",
          max_oracle_calls = 6L)
  )
  expect_false(res$complete)
  expect_warning(
    minex(code = script, granularity = "expression", backend = "inprocess",
          max_oracle_calls = 6L),
    "stopped early"
  )
})

test_that("granularity='expression' + verbose='trace' tags HDD rows with stmt_index/level", {
  script <- c("x <- 1:10",
              "result <- x |> rev() |> sqrt() |> log('oops not a base')")
  res <- minex(code = script, granularity = "expression", verbose = "trace",
               backend = "inprocess")
  expect_s3_class(res$trace, "data.frame")
  expect_true(all(c("stmt_index", "level") %in% names(res$trace)))
  # HDD rows: both stmt_index and level are populated.
  expect_true(any(!is.na(res$trace$stmt_index) & !is.na(res$trace$level)))
  # Statement-level rows (if any): stmt_index and level are NA.
  stmt_rows <- is.na(res$trace$stmt_index)
  if (any(stmt_rows)) {
    expect_true(all(is.na(res$trace$level[stmt_rows])))
  }
})

test_that("granularity='statement' + verbose='trace' trace is unchanged (no stmt_index/level)", {
  script <- c("a <- 1", "b <- 2", "log('not a number')")
  res <- minex(code = script, verbose = "trace", backend = "inprocess")
  expect_s3_class(res$trace, "data.frame")
  expect_false(any(c("stmt_index", "level") %in% names(res$trace)))
  expect_equal(names(res$trace), c("call", "phase", "size", "kept", "cached", "event"))
})

test_that("granularity='expression' with default verbose yields a NULL trace (no fabrication)", {
  script <- c("x <- 1:10",
              "result <- x |> rev() |> sqrt() |> log('oops not a base')")
  res <- minex(code = script, granularity = "expression", backend = "inprocess")
  expect_null(res$trace)
})
