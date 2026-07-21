test_that("sweep removes every individually-removable element", {
  # interesting iff element 3 present; test on index sets over 1:5.
  test <- function(idx) 3L %in% idx
  out <- remove_removable_singles(1:5, test)
  expect_equal(out, 3L)
})

test_that("sweep keeps jointly-required elements", {
  test <- function(idx) all(c(2L, 4L) %in% idx)
  out <- remove_removable_singles(1:5, test)
  expect_setequal(out, c(2L, 4L))
})

test_that("sweep reaches fixpoint when removal unlocks another removal", {
  # interesting iff sum(idx) >= 3; from {1,2,3} you can drop to a one-minimal set.
  test <- function(idx) sum(idx) >= 3
  out <- remove_removable_singles(c(1L, 2L, 3L), test)
  # one-minimal: no single element removable
  expect_true(test(out))
  for (i in seq_along(out)) expect_false(test(out[-i]))
})
