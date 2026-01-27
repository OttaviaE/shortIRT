test_that("Error when num_item is NULL", {
  set.seed(123)
  n <- 50
  theta <- rnorm(500, sd = 2)
  item_par <- data.frame(
   b = runif(n, -3, 3),
     a = runif(n, 1.2, 1.9),
     c = rep(0, n),
     e = rep(1, n)
   )
  expect_error(bench(item_par, theta = theta))
})
