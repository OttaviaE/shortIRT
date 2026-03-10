test_that("bench is a S3 class", {
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
  obj <- bench(item_pars, theta =theta, num_item = 2, K = 3)
  expect_s3_class(obj, "bench")
})
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
  expect_error(bench(item_par, theta = theta),
               regexp = "You must specify the number of items for the STFs!")
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
  expect_error(bench(item_pars, theta =theta, num_item = 2),
               regexp = "You forgot to specifiy the number of thresholds for your items!")
})
# questa cosa va fatta anche per theta target
# mettere test sul fatto che mi aspetto che il numero di item sia uguale a quello richiesto
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
  resBdicho <- bench(item_dicho, theta = theta, num_item = n_items)
  expect_equal(nrow(resBdicho$stf), n_items)

  # polytomous items
  set.seed(123)
  a1 <- runif(n, min = 0.5, max = 2)
  a2 <- runif(n, min = 0.5, max = 2)
  a3 <- runif(n, min = 0.5, max = 2)
  b_raw <- matrix(
    runif(n * 3, min = -2, max = 2),
    ncol = 3
  )
  b_sorted <- t(apply(b_raw, 1, sort))
  item_poly <- data.frame(a1, a2, a3, b1 = b_sorted[,1],
                          b2 = b_sorted[,2],
                          b3 = b_sorted[,3])
  resBpoly <- bench(item_poly, theta = theta, num_item = n_items, K = 3)
  expect_equal(nrow(resBpoly$stf), n_items)
})
