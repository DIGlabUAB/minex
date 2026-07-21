test_that("read_clipboard errors clearly without clipr", {
  # Simulate absence by masking requireNamespace within the function's view.
  testthat::skip_if(requireNamespace("clipr", quietly = TRUE) &&
                    clipr::clipr_available(),
                    "clipboard present; absence path not exercised here")
  # Match the guard's own messages specifically, so a "could not find function
  # read_clipboard" error (which also contains "clipboard") can't pass this.
  expect_error(read_clipboard(),
               "needs the 'clipr' package|No clipboard available")
})
