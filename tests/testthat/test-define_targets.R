test_that("Targets are s3 classes", {
  set.seed(123)
  theta <- rnorm(500)
  targestE <- define_targets(theta, num_targets = 10)
  expect_s3_class(targestE, "equal")
  targestC <- define_targets(theta, num_targets = 10, method = "clusters")
  expect_s3_class(targestC, "clusters")
})
test_that("Methods for defining theta targets provide different theta targets", {
  set.seed(123)
  theta <- rnorm(1000)
  targetsC <- define_targets(theta, num_targets = 5, method = "clusters")
  targetsE <- define_targets(theta, num_targets = 5, method = "equal")
  expect_false(all(targetsC == targetsE))
})
test_that("Verify that the number of targets is equal to the desired number", {
  set.seed(123)
  theta <- rnorm(1000)
  n_targets <- 5
  targetsE <- define_targets(theta, num_targets = n_targets,
                             method = "equal")
  targetsC <- define_targets(theta, num_targets = n_targets,
                             method = "clusters")
  expect_equal(length(targetsC), n_targets)
})
test_that("Equal intervals are equal", {
  set.seed(123)
  theta <- rnorm(500)
  targets <- define_targets(theta, num_targets = 4)
  d <- diff(x)
  expect_equal(length(unique(d)), 1)
})

