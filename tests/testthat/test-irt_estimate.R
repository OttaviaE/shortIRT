test_that("The estimates of Theta have the same length as the original theta values", {
  set.seed(123)
  n <- 50
  theta <- rnorm(500)
  item_par <- data.frame(
     b = runif(n, -3, 3),
     a = runif(n, 1.2, 1.9),
     c = rep(0, n),
     e = rep(1, n)
   )
   # estimate theta
  theta_hat <- irt_estimate(item_par, theta = theta)
  expect_equal(length(theta_hat), length(theta))
})
