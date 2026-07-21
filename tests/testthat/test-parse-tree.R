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
