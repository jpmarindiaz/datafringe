context("sample data")

test_that("Sample Data", {
  t <- sampleData("Ca-Nu", asFringe = TRUE)
  expect_equal(getFtype(t),"Ca-Nu")
  expect_error(sampleData("XXXXXX", asFringe = TRUE))
  t <- sampleData("Ca-Nu", gt0 = FALSE)
  expect_true(any(t$b < 0))

  t <- sampleData("Ca-NuP")

})
