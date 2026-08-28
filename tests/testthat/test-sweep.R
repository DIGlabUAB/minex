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

test_that("sweep requires a second outer pass when removal unlocks an earlier index", {
  # interesting iff 3 is present without 2.
  # Single inner pass over c(1,2,3): index 1 (value 1) is checked first and is
  # NOT removable yet (test(c(2,3)) is FALSE), so the while advances past it.
  # Removing value 2 next (test(c(1,3)) is TRUE) then makes value 1 removable
  # (test(c(3)) is TRUE) -- but the inner while has already moved past index 1
  # and never revisits it. Only a second full pass, started by the outer
  # repeat, can catch and remove it.
  test <- function(idx) (3L %in% idx) && !(2L %in% idx)
  out <- remove_removable_singles(c(1L, 2L, 3L), test)
  expect_equal(out, 3L)
  # one-minimal: no single element removable
  expect_true(test(out))
})
