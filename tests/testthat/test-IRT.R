test_that("mpirt is an S3 class", {
  set.seed(123)
  n <- 50
  theta <- rnorm(500)
  item_pars <- data.frame(
     b = runif(n, -3, 3),
     a = runif(n, 1.2, 1.9),
     c = rep(0, n),
     e = rep(1, n)
   )
  expected_prob <- mpirt(item_pars, theta)
  expect_s3_class(expected_prob, "mpirt")
})
test_that("IRT computes correct probability", {
  expect_equal(IRT(theta = 0,  b = 0, a = 1,c = 0, e = 1),
               0.50)
})
test_that("IRT stops if no theta value is provided", {
  expect_error(IRT( b = 0, a = 1,c = 0, e = 1))
})
test_that("The probability for multiple items is computed", {
  set.seed(123)
  n <- 10
  theta <- seq(-3,3, length.out=500)
  item_par <- data.frame(a = runif(n, .90, 1.70),
                         b = runif(n, -3, 3),
                         c = runif(n, .00, .10),
                         e = rep(1, n))
  myp <- mpirt(item_par, theta = theta)
  expect_equal(nrow(myp), length(theta))
  expect_equal(ncol(myp), n)

})
test_that("Number of item is equal for probabilities and simulate responses", {
  set.seed(123)
  n <- 10
  theta <- seq(-3,3, length.out=500)
  item_par <- data.frame(a = runif(n, .90, 1.70),
                         b = runif(n, -3, 3),
                         c = runif(n, .00, .10),
                         e = rep(1, n))
  myp <- mpirt(item_par, theta = theta)
  myo <- obsirt(myp)
  expect_equal(ncol(myp), ncol(myo))
})
