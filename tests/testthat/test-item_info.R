test_that("iifs is an S3 class", {
  set.seed(123)
  parameters <- data.frame(b = c(-3,-2,0, 2, 3),
   a = runif(5, 1.2, 1.9),
   c = rep(0,5),
   e= rep(1, 5))
  # compute information for dichomtous items with default theta values
  infos <- item_info(parameters)
  expect_s3_class(infos, "iifs")
  # 4 items with 4 response categories (K = 3)
  item_pars <- data.frame(matrix(c(
           1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
           0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
          0.5, 1.5, 1, -1.5, -1.0, 0,
          1, 1, 1, -1.5, -0, 0.5),
          nrow = 4,
          byrow = TRUE))
  colnames(item_pars) <- paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
  info_poly <- item_info(item_pars, K = 3)
  expect_s3_class(info_poly, "iifs")
})
test_that("tif is an S3 class", {
  set.seed(123)
  parameters <- data.frame(b = c(-3,-2,0, 2, 3),
                           a = runif(5, 1.2, 1.9),
                           c = rep(0,5),
                           e= rep(1, 5))
  # compute information for dichomtous items with default theta values
  tif <- tif(item_info(parameters))
  expect_s3_class(tif, "tif")
  # 4 items with 4 response categories (K = 3)
  item_pars <- data.frame(matrix(c(
    1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
    0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
    0.5, 1.5, 1, -1.5, -1.0, 0,
    1, 1, 1, -1.5, -0, 0.5),
    nrow = 4,
    byrow = TRUE))
  colnames(item_pars) <- paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
  tif_poly <- tif(item_info(item_pars, K = 3))
  expect_s3_class(tif_poly, "tif")
})
test_that("The number of categories are defined for polytomous items", {
  item_pars <- data.frame(a1 = 1.2, a2 = 1.0, a3= 0.8,
                           b1 = -1.0, b2 = 0.0, b3 = 1.2)
  expect_error(item_info(item_pars = item_pars))
})
test_that("Item information Function is positive", {
  # dichotomous responses
  item_par <- data.frame(b = 0, a = 1.3, c= .20, e = .98)
  myinfo <- i_info(item_par,
                   theta = seq(-5,5, length.out = 1000))
  expect_true(all(myinfo > 0))
  # polytomous responses
  item_pars <- data.frame(a1 = 1.2, a2 = 1.0, a3= 0.8,
                          b1 = -1.0, b2 = 0.0, b3 = 1.2)
  myinfo <- i_info(item_pars,
                   theta = seq(-5,5, length.out = 1000), K = 3)
  expect_true(all(myinfo > 0))
})
test_that("Test Information Function is positive", {
  # dichotomous responses
   set.seed(123)
   n <- 5
   item_par <- data.frame(
     b = runif(n, -3, 3),
     a = runif(n, 1.2, 1.9),
     c = rep(0, n),
     e = rep(1, n)
   )
   iifs <- item_info(item_par)
   test_tif <- tif(iifs)
   expect_true(all(test_tif$tif >0))
   # polytomous responses
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
   iifs <- item_info(item_pars, K = 3)
   test_tif <- tif(iifs)
   expect_true(all(test_tif$tif >0))
})
