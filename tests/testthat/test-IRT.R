test_that("IRT computes correct probability", {
  expect_equal(IRT(theta = 0,  b = 0, a = 1,c = 0, e = 1),
               0.50)
})
test_that("IRT stops if no theta value is provided", {
  expect_error(IRT( b = 0, a = 1,c = 0, e = 1))
})
