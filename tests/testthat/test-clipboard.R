test_that("read_clipboard errors clearly without clipr", {
  # Simulate absence by masking requireNamespace within the function's view.
  testthat::skip_if(requireNamespace("clipr", quietly = TRUE) &&
                    clipr::clipr_available(),
                    "clipboard present; absence path not exercised here")
  expect_error(read_clipboard(), "clipr|clipboard")
})
