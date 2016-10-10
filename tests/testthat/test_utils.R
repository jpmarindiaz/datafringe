context("utils")

test_that("Guess Ctypes", {
  v <- c("1",NA,"2")
  expect_equal(guessCtype(v),"Nu")
})

