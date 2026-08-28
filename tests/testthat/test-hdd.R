test_that("hdd_star isolates the failing pipeline stage", {
  # reproduces = "the source still contains 'log('"
  txt <- "x |> rev() |> sqrt() |> log()"
  out <- hdd_star(txt, function(s) grepl("log(", s, fixed = TRUE),
                  max_oracle_calls = Inf, info = new.env())
  expect_true(grepl("log(", out, fixed = TRUE))
  expect_false(grepl("rev(", out, fixed = TRUE))
  expect_false(grepl("sqrt(", out, fixed = TRUE))
  expect_true(pt_parses(out))
})

test_that("hdd_star isolates a positional argument", {
  txt <- "f(aa, bb, cc)"
  out <- hdd_star(txt, function(s) grepl("bb", s, fixed = TRUE),
                  max_oracle_calls = Inf, info = new.env())
  expect_true(grepl("bb", out, fixed = TRUE))
  expect_false(grepl("aa", out, fixed = TRUE))
  expect_false(grepl("cc", out, fixed = TRUE))
  expect_true(pt_parses(out))
})

test_that("nested arg and its parent never break reduction (per-depth levels)", {
  # f(g(bad), ok): failure needs 'bad'. Parent g(bad) at a shallower depth than 'bad'.
  # Verified reduced output: "f(g(bad))" -- the irrelevant sibling 'ok' is
  # dropped while the parent call wrapping 'bad' survives.
  txt <- "f(g(bad), ok)"
  out <- hdd_star(txt, function(s) grepl("bad", s, fixed = TRUE),
                  max_oracle_calls = Inf, info = new.env())
  expect_true(grepl("bad", out, fixed = TRUE))
  expect_false(grepl("ok", out, fixed = TRUE))
  expect_true(grepl("g(", out, fixed = TRUE))
  expect_true(pt_parses(out))
})

test_that("a fully-irrelevant level is emptied (ddmin cannot reach empty keep)", {
  # Culprit is the NAMED arg value, not a positional arg: BOTH positional args
  # aa and cc are removable. ddmin's non-empty guarantee would leave one behind;
  # the empty-keep probe must delete both.
  out <- hdd_star("f(aa, y = 2, cc)", function(s) grepl("y = 2", s, fixed = TRUE),
                  max_oracle_calls = Inf, info = new.env())
  expect_true(grepl("y = 2", out, fixed = TRUE))
  expect_false(grepl("aa", out, fixed = TRUE))
  expect_false(grepl("cc", out, fixed = TRUE))
  expect_true(pt_parses(out))

  # Culprit is the pipe DATA SOURCE: every stage is removable.
  out2 <- hdd_star("x |> rev() |> sqrt()", function(s) grepl("x", s, fixed = TRUE),
                   max_oracle_calls = Inf, info = new.env())
  expect_false(grepl("rev(", out2, fixed = TRUE))
  expect_false(grepl("sqrt(", out2, fixed = TRUE))
  expect_true(pt_parses(out2))
})

test_that("comment guard excludes a span whose LATER line (not just its start line) has a trailing comment", {
  # "x |>\n  f(bad, ok) # note": the pipe-stage span for `f(bad, ok)` starts on
  # clean line 1 (the `|>` operator) but its body crosses line 2, which carries
  # a trailing `# note` comment. A guard that only checks the span's start
  # line misses this and lets the span through; verified pre-fix this
  # returned a non-empty level (length 1). The fix must check every line the
  # span traverses, so the level is emptied entirely.
  txt <- "x |>\n  f(bad, ok) # note"
  expect_length(hdd_reducible_by_depth(txt), 0L)
})

test_that("hdd_make_oracle memoizes by source string", {
  calls <- 0L
  base <- function(s) { calls <<- calls + 1L; grepl("bad", s, fixed = TRUE) }
  oracle <- hdd_make_oracle(base)
  oracle("f(bad)"); oracle("f(bad)"); oracle("f(ok)")
  expect_equal(calls, 2L)   # "f(bad)" tested once, "f(ok)" once
})

test_that("hdd_star honours a tight budget and reports incomplete", {
  txt <- "f(a, b, c, d, e)"
  info <- new.env()
  out <- hdd_star(txt, function(s) grepl("c", s, fixed = TRUE),
                  max_oracle_calls = 1L, info = info)
  expect_false(info$complete)
  expect_true(pt_parses(out))
})
