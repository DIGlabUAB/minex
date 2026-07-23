test_that("is_denylisted flags side-effecting calls and clears benign code", {
  expect_true(is_denylisted("system('rm -rf /')"))
  expect_true(is_denylisted("x <- 1\ndownload.file(u, 'f')"))
  expect_true(is_denylisted("pipe('sh')"))
  expect_true(is_denylisted("writeLines(x, '~/.bashrc')"))
  expect_false(is_denylisted("m <- mean(x, na.rm = TRUE)"))
  expect_false(is_denylisted("y <- sqrt(sum(1:10))"))
})

test_that("target_type passes non-'any' through and resolves 'any' from recorded conditions", {
  # Recorded condition is an ERROR, so pass-through and 'any'-resolution DIFFER --
  # this kills a constant-return stub and forces both branches.
  target <- list(
    message = "boom", classes = c("simpleError", "error"),
    conditions = list(list(type = "error", message = "boom",
                           classes = c("simpleError", "error"), stmt_index = 1L))
  )
  expect_equal(target_type(target, "warning"), "warning")  # pass-through, unrelated to recorded
  expect_equal(target_type(target, "error"), "error")      # pass-through
  expect_equal(target_type(target, "any"), "error")        # resolved from the recorded condition
})

# ---------------------------------------------------------------------------
# verify_fix_code()
# ---------------------------------------------------------------------------

# helper: an error target for "stop('boom')"
boom_target <- function() list(
  message = "boom", classes = c("simpleError", "error", "condition"),
  conditions = list(list(type = "error", message = "boom",
                         classes = c("simpleError", "error", "condition"),
                         stmt_index = 1L))
)

test_that("verify_fix_code TRUE only on a clean run", {
  v <- verify_fix_code("x <- 1", boom_target(), "error", "message", backend = "inprocess")
  expect_true(isTRUE(v$verified))
})

test_that("verify_fix_code FALSE when the same failure persists", {
  v <- verify_fix_code("stop('boom')", boom_target(), "error", "message", backend = "inprocess")
  expect_false(v$verified)
  expect_match(v$verification, "same failure")
})

test_that("verify_fix_code FALSE on a different error", {
  v <- verify_fix_code("stop('other')", boom_target(), "error", "message", backend = "inprocess")
  expect_false(v$verified)
  expect_match(v$verification, "different")
})

test_that("verify_fix_code FALSE for a warning target whose fix now errors", {
  wt <- list(message = "w", classes = c("simpleWarning", "warning", "condition"),
             conditions = list(list(type = "warning", message = "w",
                                    classes = c("simpleWarning","warning","condition"),
                                    stmt_index = 1L)))
  v <- verify_fix_code("stop('crash')", wt, "warning", "message", backend = "inprocess")
  expect_false(v$verified)   # not NA, not TRUE
})

test_that("verify_fix_code NA for empty/comment-only, denylisted, unparseable, custom-oracle", {
  expect_true(is.na(verify_fix_code("# upgrade to >= 2.0", boom_target(), "error", "message", backend = "inprocess")$verified))
  expect_true(is.na(verify_fix_code("system('x')", boom_target(), "error", "message", backend = "inprocess")$verified))
  expect_true(is.na(verify_fix_code("x <- (", boom_target(), "error", "message", backend = "inprocess")$verified))
  expect_true(is.na(verify_fix_code("x <- 1", NULL, "error", "message", backend = "inprocess")$verified))
})

test_that("verify_fix_code splits statements so a leading library() cannot mask a failure", {
  v <- verify_fix_code("invisible(1)\nstop('boom')", boom_target(), "error", "message", backend = "inprocess")
  expect_false(v$verified)   # would be TRUE if only the first statement ran
})

test_that("verify_fix_code exercises the real callr backend (clean -> TRUE, broken -> FALSE)", {
  skip_on_cran()   # spawns real subprocesses; too slow for CRAN
  expect_true(isTRUE(verify_fix_code("x <- 1", boom_target(), "error", "message")$verified))   # default backend = "callr"
  expect_false(verify_fix_code("stop('boom')", boom_target(), "error", "message")$verified)
})