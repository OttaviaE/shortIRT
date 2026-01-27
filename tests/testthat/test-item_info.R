test_that("Item information Function is positive", {
  myinfo <- i_info(b = 0, a = 1.3, c= .20, e = .98,
                   theta = seq(-5,5, length.out = 1000))
  expect_true(all(myinfo > 0))
})
test_that("Test Information Function is positive", {
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
})
