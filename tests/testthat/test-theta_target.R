test_that("theta_target is a S3 class", {
  set.seed(123)
  theta <- rnorm(500, sd = 2)
  item_pars <- data.frame(matrix(
    c(
      1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
      0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
      0.5, 1.5, 1, -1.5, -1.0, 0,
      1, 1, 1, -1.5, -0, 0.5
    ),
    nrow = 4,
    byrow = TRUE
  ))
  colnames(item_pars) = paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
  obj <- theta_target(c(-2, -1), item_pars, theta =theta, K = 3)
  expect_s3_class(obj, "theta_target")
})
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
test_that("Error when item are polytomous and the number of thresholds is missing", {
  set.seed(123)
  theta <- rnorm(500, sd = 2)
  item_pars <- data.frame(matrix(
    c(
      1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
      0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
      0.5, 1.5, 1, -1.5, -1.0, 0,
      1, 1, 1, -1.5, -0, 0.5
    ),
    nrow = 4,
    byrow = TRUE
  ))
  colnames(item_pars) = paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
  expect_error(theta_target(c(-2, -1, 0), item_pars, theta =theta),
               regexp = "You forgot to specifiy the number of thresholds for your items!")
})
test_that("Number of selected items is the same as the requsted one", {
  n_items <- 3
  set.seed(123)
  # dichotmous items
  n <- 50
  theta <- rnorm(500, sd = 2)
  item_dicho <- data.frame(
    b = runif(n, -3, 3),
    a = runif(n, 1.2, 1.9),
    c = rep(0, n),
    e = rep(1, n)
  )
  targetsE <- define_targets(theta, num_targets = 3)
  targetsC <- define_targets(theta, num_targets = 3, method = "cluster")
  resTdichoE <- theta_target(targetsE, item_dicho, theta =theta)
  resTdichoC <- theta_target(targetsC, item_dicho, theta =theta)
  expect_equal(nrow(resTdichoE$stf), n_items)
  expect_equal(nrow(resTdichoC$stf), n_items)
  # polytomous items
  set.seed(123)
  a1 <- runif(n, min = 0.5, max = 2)
  a2 <- runif(n, min = 0.5, max = 2)
  a3 <- runif(n, min = 0.5, max = 2)
  # ---- 2. Difficoltà (uniformi) ----
  b_raw <- matrix(
    runif(n * 3, min = -2, max = 2),
    ncol = 3
  )
  b_sorted <- t(apply(b_raw, 1, sort))
  # ---- 4. Costruisco la matrice finale ----
  item_poly <- data.frame(a1, a2, a3, b1 = b_sorted[,1],
                          b2 = b_sorted[,2],
                          b3 = b_sorted[,3])
  resTpolyE <- theta_target(targetsE, item_poly, theta =theta)
  resTpolyC <- theta_target(targetsC, item_poly, theta =theta)
  expect_equal(nrow(resTpolyE$stf), n_items)
  expect_equal(nrow(resTpolyC$stf), n_items)
})
