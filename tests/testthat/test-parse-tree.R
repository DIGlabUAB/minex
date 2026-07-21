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
