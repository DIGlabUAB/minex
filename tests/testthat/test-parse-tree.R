test_that("pt_data returns getParseData with source kept", {
  pd <- pt_data("f(a, b)")
  expect_s3_class(pd, "data.frame")
  expect_true(all(c("line1","col1","line2","col2","parent","token","text") %in% names(pd)))
  expect_true(any(pd$token == "SYMBOL" & pd$text == "a"))
})

test_that("pt_data errors on unparseable input", {
  expect_error(pt_data("f(a, "))
})

test_that("pt_depth gives nested nodes greater depth than their parents", {
  pd <- pt_data("f(g(a), b)")
  d <- pt_depth(pd)
  # the SYMBOL 'a' (inside g(...)) must be deeper than SYMBOL 'b' (top-level arg)
  da <- d[pd$token == "SYMBOL" & pd$text == "a"]
  db <- d[pd$token == "SYMBOL" & pd$text == "b"]
  expect_true(length(da) == 1 && length(db) == 1)
  expect_gt(da, db)
})

test_that("pt_is_pipe_row detects |> and %>% but not %in%", {
  pd <- pt_data("x |> a()")
  expect_true(any(pt_is_pipe_row(pd)))
  pd2 <- pt_data("x %>% a()")
  expect_true(any(pt_is_pipe_row(pd2)))
  pd3 <- pt_data("a %in% b")
  expect_false(any(pt_is_pipe_row(pd3)))   # %in% is SPECIAL but not a pipe
})

test_that("pt_pipe_stages flattens the spine; data source is non-reducible", {
  st <- pt_pipe_stages("x |> rev() |> sqrt()")[[1]]
  expect_equal(length(st), 3L)               # x, rev(), sqrt()
  expect_false(st[[1]]$reducible)            # data source x
  expect_true(st[[2]]$reducible)
  expect_true(st[[3]]$reducible)
})

test_that("pt_pipe_stages offsets round-trip: deleting a reducible span reparses to the reduced chain", {
  text <- "x |> rev() |> sqrt()"
  st <- pt_pipe_stages(text)[[1]]

  delete_span <- function(text, span) {
    paste0(substr(text, 1, span$from - 1L), substr(text, span$to + 1L, nchar(text)))
  }

  # Removing stage 2 ("|> rev()") should leave something equivalent to `x |> sqrt()`
  after_removing_stage2 <- delete_span(text, st[[2]])
  expect_silent(parsed2 <- parse(text = after_removing_stage2, keep.source = FALSE))
  expect_equal(deparse(parsed2[[1]]), deparse(parse(text = "x |> sqrt()")[[1]]))

  # Removing stage 3 ("|> sqrt()") should leave something equivalent to `x |> rev()`
  after_removing_stage3 <- delete_span(text, st[[3]])
  expect_silent(parsed3 <- parse(text = after_removing_stage3, keep.source = FALSE))
  expect_equal(deparse(parsed3[[1]]), deparse(parse(text = "x |> rev()")[[1]]))
})

test_that("pt_pipe_stages offsets round-trip on a 3-operator chain (multi-level chain_key climb)", {
  # A 3-operator chain forces chain_key's `repeat` loop to climb multiple
  # ancestor levels through is_pipe_expr before finding the chain root.
  text <- "a |> b() |> c() |> d()"
  st <- pt_pipe_stages(text)[[1]]
  expect_equal(length(st), 4L)                # a, b(), c(), d()
  expect_false(st[[1]]$reducible)              # data source a
  expect_true(st[[2]]$reducible)
  expect_true(st[[3]]$reducible)
  expect_true(st[[4]]$reducible)

  delete_span <- function(text, span) {
    paste0(substr(text, 1, span$from - 1L), substr(text, span$to + 1L, nchar(text)))
  }

  # Removing stage 2 ("|> b()") should leave something equivalent to d(c(a))
  after_removing_stage2 <- delete_span(text, st[[2]])
  expect_silent(parsed2 <- parse(text = after_removing_stage2, keep.source = FALSE))
  expect_equal(deparse(parsed2[[1]]), deparse(parse(text = "d(c(a))")[[1]]))

  # Removing stage 3 ("|> c()") should leave something equivalent to d(b(a))
  after_removing_stage3 <- delete_span(text, st[[3]])
  expect_silent(parsed3 <- parse(text = after_removing_stage3, keep.source = FALSE))
  expect_equal(deparse(parsed3[[1]]), deparse(parse(text = "d(b(a))")[[1]]))

  # Removing stage 4 ("|> d()") should leave something equivalent to c(b(a))
  after_removing_stage4 <- delete_span(text, st[[4]])
  expect_silent(parsed4 <- parse(text = after_removing_stage4, keep.source = FALSE))
  expect_equal(deparse(parsed4[[1]]), deparse(parse(text = "c(b(a))")[[1]]))
})

del <- function(text, span) paste0(substr(text, 1, span$from - 1L),
                                   substr(text, span$to + 1L, nchar(text)))

test_that("positional-arg deletion parses for every position", {
  t <- "f(a, b, c)"
  spans <- pt_call_args(t)[[1]]
  results <- vapply(spans, function(s) del(t, s), character(1))
  # each single deletion must parse and be one of the expected reductions
  ok <- vapply(results, function(r) !inherits(try(parse(text=r), silent=TRUE), "try-error"),
               logical(1))
  expect_true(all(ok))
  expect_setequal(gsub(" ", "", results), c("f(b,c)", "f(a,c)", "f(a,b)"))
})

test_that("single positional arg deletes to f()", {
  t <- "f(a)"
  r <- del(t, pt_call_args(t)[[1]][[1]])
  expect_equal(gsub(" ", "", r), "f()")
})

test_that("last positional after a named arg uses the preceding comma", {
  t <- "f(y = 1, a)"                    # only `a` is positional
  spans <- pt_call_args(t)[[1]]
  expect_equal(length(spans), 1L)
  r <- del(t, spans[[1]])
  expect_equal(gsub(" ", "", r), "f(y=1)")   # NOT "f(y = 1," (which wouldn't parse)
})

test_that("grouping parens are not mistaken for a call", {
  t <- "(a + b)"
  out <- pt_call_args(t)
  expect_equal(length(out), 0L)   # no call/args detected at all
})

test_that("if-condition parens are skipped but a real nested call is still detected", {
  t <- "if (x > 1) foo(a, b)"
  out <- pt_call_args(t)
  # exactly one call detected: foo(a, b) -- the if()'s '(' is not a call
  expect_equal(length(out), 1L)
  spans <- out[[1]]
  expect_equal(length(spans), 2L)
  results <- vapply(spans, function(s) del(t, s), character(1))
  ok <- vapply(results, function(r) !inherits(try(parse(text = r), silent = TRUE), "try-error"),
               logical(1))
  expect_true(all(ok))
  expect_setequal(gsub(" ", "", results), c("if(x>1)foo(b)", "if(x>1)foo(a)"))
})

test_that("for-loop parens are skipped but a real nested call is still detected", {
  t <- "for (i in 1:3) g(a, b)"
  out <- pt_call_args(t)
  # exactly one call detected: g(a, b) -- the for()'s '(' is not a call
  expect_equal(length(out), 1L)
  spans <- out[[1]]
  expect_equal(length(spans), 2L)
  results <- vapply(spans, function(s) del(t, s), character(1))
  ok <- vapply(results, function(r) !inherits(try(parse(text = r), silent = TRUE), "try-error"),
               logical(1))
  expect_true(all(ok))
  expect_setequal(gsub(" ", "", results), c("for(iin1:3)g(b)", "for(iin1:3)g(a)"))
})

test_that("namespaced and method-style calls still detect positional args", {
  t1 <- "pkg::fn(a, b)"
  out1 <- pt_call_args(t1)
  expect_equal(length(out1), 1L)
  expect_equal(length(out1[[1]]), 2L)

  t2 <- "x$m(a, b)"
  out2 <- pt_call_args(t2)
  expect_equal(length(out2), 1L)
  expect_equal(length(out2[[1]]), 2L)
})

test_that("pt_delete removes a span and trims residual whitespace", {
  t <- "x |> rev() |> sqrt()"
  st <- pt_pipe_stages(t)[[1]]
  out <- pt_delete(t, list(list(from = st[[3]]$from, to = st[[3]]$to)))  # drop sqrt()
  expect_true(pt_parses(out))
  expect_false(grepl("  ", out))          # no double space residue
  expect_equal(gsub(" ", "", out), "x|>rev()")
})

test_that("pt_parses rejects a dangling operator", {
  expect_false(pt_parses("x |>  |> sqrt()"))
  expect_true(pt_parses("x |> sqrt()"))
})

test_that("a line with a trailing comment is flagged", {
  pd <- pt_data("f(a) # note")
  expect_true(any(pd$token == "COMMENT"))
})
