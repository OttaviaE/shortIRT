test_that("The number of selected items is equal to the number of targets", {
   set.seed(123)
   n <- 50
   theta <- rnorm(500)
   item_par <- data.frame(
     b = runif(n, -3, 3),
     a = runif(n, 1.2, 1.9),
     c = rep(0, n),
     e = rep(1, n)
   )
   targets <- define_targets(theta, num_targets = 4)
   resT <- theta_target(targets, item_par)
   expect_equal(nrow(resT$stf), length(targets))
})
