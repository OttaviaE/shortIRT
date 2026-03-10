test_that("isa is a S3 class", {
  set.seed(123)
  n <- 50
  nmin <- 9
  theta <- rnorm(500)

  item_pars <- data.frame(
    b = runif(n, -3, 3),
    a = runif(n, 1.2, 1.9),
    c = rep(0, n),
    e = rep(1, n)
  )

  tif_target <- tif(
    item_info(item_pars[-c(3, 5, 10, 14), ]),
    fun = "mean"
  )

  obj <- isa(item_pars, tif_target, nmin = nmin)
  expect_s3_class(obj, "isa")
})
test_that("ISA returns a STF composed of a number of items greater or equal to the defined one", {
  # dichotmous respones
  set.seed(123)
  n <- 50
  nmin <- 9
  theta <- rnorm(500)

  item_pars <- data.frame(
     b = runif(n, -3, 3),
     a = runif(n, 1.2, 1.9),
     c = rep(0, n),
     e = rep(1, n)
   )

  tif_target <- tif(
     item_info(item_pars[-c(3, 5, 10, 14), ]),
     fun = "mean"
   )

  resIsa <- isa(item_pars, tif_target, nmin = nmin)
  expect_gte(nrow(resIsa$stf), unique(resIsa$stf$nmin))

  # polytomous responses
  set.seed(123)
  n_items <- 50
  nmin <- 9
  a1 <- runif(n_items, min = 0.5, max = 2)
  a2 <- runif(n_items, min = 0.5, max = 2)
  b_raw <- matrix(
    runif(n_items * 2, min = -2, max = 2),
    ncol = 2
  )
  b1 <- pmin(b_raw[,1], b_raw[,2])
  b2 <- pmax(b_raw[,1], b_raw[,2])
  item_pars <- data.frame(a1, a2, b1, b2)
  tif_target <- tif(
    item_info(item_pars[-c(3, 5, 10, 14), ], K = 2),
    fun = "mean"
  )

  resIsa <- isa(item_pars, tif_target, nmin = nmin, K = 2)
  expect_gte(nrow(resIsa$stf), unique(resIsa$stf$nmin))
})

