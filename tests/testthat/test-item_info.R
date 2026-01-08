test_that("i_info computes the probability correctly", {
  expect_equal(i_info(b = 0, theta = 0), .25)
})
test_that("pseuoguessing lower the IIF", {
  expect_lt(i_info(b = 0, c= .10, theta = 0), .25)
})
test_that("upper asymptote lower the IIF", {
  expect_lt(i_info(b = 0, e= .90, theta = 0), .25)
})
test_that("pseuoguessing lower the IIF also with discrimination", {
  expect_lt(i_info(b = 0, c= .10, a = 1.2, theta = 0),
            i_info(b = 0,  a = 1.2, theta = 0))
})
test_that("upper asymptote lower the IIF also with discrimination", {
  expect_lt(i_info(b = 0, e= .90, a = 1.2, theta = 0),
            i_info(b = 0, a = 1.2, theta = 0))
})
test_that("item_info computes the probability correctly for all items", {
  set.seed(123)
  parameters <- data.frame(b = c(-3,-2,0, 2, 3),
                           a = runif(5, 1.2, 1.9),
                           c = rep(0,5),
                           e = rep(1, 5))
  infos <- item_info(parameters)
  expect_equal(ncol(infos), nrow(parameters))
})
