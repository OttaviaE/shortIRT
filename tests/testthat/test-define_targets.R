test_that("Methods for defining theta targets provide different theta targets", {
  set.seed(123)
  theta <- rnorm(1000)
  targetsC <- define_targets(theta, num_targets = 5, method = "clusters")
  targetsE <- define_targets(theta, num_targets = 5, method = "equal")
  expect_false(all(targetsC == targetsE))
})
test_that("Verify that the number of targets is equal to the desired number Equal", {
  set.seed(123)
  theta <- rnorm(1000)
  n_targets <- 5
  targetsE <- define_targets(theta, num_targets = n_targets,
                             method = "equal")
  expect_equal(length(targetsE), n_targets)
})
