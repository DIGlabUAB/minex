test_that("explanation_type builds an ellmer type object", {
  skip_if_not_installed("ellmer")
  ty <- explanation_type()
  expect_true(inherits(ty, "ellmer::TypeObject"))
})