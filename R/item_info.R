
#' Itenm information function (single item)
#'
#' Compute the item information function for a single item. The formula for the computation is taken from REFRENCE d magis
#'
#' @param b location of item i, with no default.
#' @param a discrimination parameter for item i. Default is 1.
#' @param c pesudoguessing parameter of item i. Default is 0.
#' @param e Upper asymptote of item i. Default is 1.
#' @param theta numeric vector (might also be a single value) representing the latent trait. Default is a vector of 1 thousand values rangin from -5 to +5
#'
#' @returns A numeric vector of length equal to theta, which contains the IIF for a single item with respect to the values specified in theta
#' @export
#'
#' @examples
#' # IIF of an item with b = 0
#' i_info(b = 0, theta = c(-3,-1,0,1,3))
i_info <- function(b, a=1,c=0, e= 1,
                   theta = seq(-5,5,length.out=1000)){
  P <- IRT(theta, b = b, a = a, e = e, c=c)
  Q <- 1 - P
  num <- (a^2)*((P-c)^2)*((e-P)^2)
  den <- ((e-c)^2)*P*Q
  Ii <- num/den
  return(Ii)
}

#' Item Information function (multiple items)
#'
#' @param ipar dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective parameters.
#' @param theta numeric vector (might also be a single value) representing the latent trait. Default is a vector of 1 thousand values ranging from -5 to +5
#'
#' @returns A matrix with nrows equal to the length of theta and ncols equal to the number of items in ipar.
#' @export
#'
#' @examples
#' set.seed(123)
#' parameters <- data.frame(b = c(-3,-2,0, 2, 3),
#' a = runif(5, 1.2, 1.9),
#' c = rep(0,5),
#' e= rep(1, 5))
#' infos <- item_info(parameters)
#' head(infos)
item_info <- function(ipar, theta = seq(-5,5,length.out=1000)){
  item <- lapply(1:nrow(ipar), function(i) {
    i_info(
      b = ipar[i, "b"],
      a = ipar[i, "a"],
      c = ipar[i, "c"],
      e = ipar[i, "e"],
      theta = theta
    )
  })
  item <- data.frame(do.call(cbind, item))
  if (length(unique(theta)) == 1) {
    theta <- paste(theta, 1:length(theta), sep = "-")
  }
  rownames(item) <- theta
  colnames(item) <- rownames(ipar)
  item <- structure(
    item,
    class = c("iifs", class(item))
  )
  return(item)
}

#' Test Information Function
#'
#'  Computes the test information function of a test given a matrix of IIF
#'
#' @param iifs matrix or dataframe with nrows equal to the levesl of theta and ncols equal to the number of items in a test
#' @param fun character, defines the function for the computation of the TIF, either by summing the items (sum) or by computing the mean (mean)
#' @param theta numeric vector (might also be a single value) representing the latent trait. Default is a vector of 1 thousand values rangin from -5 to +5
#'
#' @returns A dataframe with two columns: theta with the values of theta, either default values or defined by the user, and the value of the TIF
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 5
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' iifs <- item_info(item_par)
#' test_tif <- tif(iifs)
tif <- function(iifs, fun = "sum",
                theta = seq(-5,5,length.out=1000)) {
  tif_fun <- switch(fun,
                    sum = rowSums,
                    mean = rowMeans)
  thetif <- data.frame(theta,
                       tif = tif_fun(iifs))
  class(thetif) <- "tif"
  attr(thetif, "source") <- fun
  return(thetif)
}
