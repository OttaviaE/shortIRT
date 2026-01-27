#' Item Information Function (single item, IIF)
#'
#' Compute the item information function for a single item. See \code{Details}.
#'
#' @param b \code{numeric}, difficulty/location parameter \eqn{b_i}
#' @param a \code{numeric}, discrimination parameter \eqn{a_i}. Default is 1.
#' @param c \code{numeric}, pesudoguessing parameter \eqn{c_i}. Default is 0.
#' @param e \code{numeric}, upper asymptote \eqn{e_i}. Default is 1.
#' @param theta \code{numeric} latent trait level of person \eqn{p}, it can be a single value or a vector of values. Default is a vector of 1 thousand values rangin from -5 to +5
#' @details
#' Let \eqn{P(\theta)} denote the probability of a correct response under the
#' four-parameter logistic (4PL) model:
#'
#' \deqn{
#' P(\theta) =
#' c + \frac{e - c}{1 + \exp\left[-a(\theta - b)\right]}
#' }
#'
#' and let \eqn{Q(\theta) = 1 - P(\theta)}.
#'
#' The item information function is computed as:
#'
#' \deqn{
#' I(\theta) =
#' \frac{a^2 \left[P(\theta) - c\right]^2 \left[e - P(\theta)\right]^2}
#' {(e - c)^2 \, P(\theta) \, Q(\theta)}
#' }
#'
#' @returns A numeric vector of length equal to \code{theta}, which contains the item information function for a single item with respect to the values specified in theta
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

#' Item Information Functions (multiple items, IIFs)
#'
#' Computes the item information functions for multiple items
#'
#' @param item_par \code{data.frame}, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#' @param theta \code{numeric} latent trait level of person \eqn{p}, it can be a single value or a vector of values.
#'
#' @returns A \code{matrix} of class \code{iifs} with nrows equal to the length of \code{theta} and ncols equal to the number of items in \code{item_par}
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
item_info <- function(item_par, theta = seq(-5,5,length.out=1000)){
  item <- lapply(1:nrow(item_par), function(i) {
    i_info(
      b = item_par[i, "b"],
      a = item_par[i, "a"],
      c = item_par[i, "c"],
      e = item_par[i, "e"],
      theta = theta
    )
  })
  item <- data.frame(do.call(cbind, item))
  if (length(unique(theta)) == 1) {
    theta <- paste(theta, 1:length(theta), sep = "-")
  }
  rownames(item) <- theta
  colnames(item) <- rownames(item_par)
  item <- structure(
    item,
    class = c("iifs", class(item))
  )
  return(item)
}

#' Test Information Function (TIF)
#'
#'  Compute the test information function of a test given a matrix of item information functions
#'
#' @param iifs object of class \code{iifs} containg the item information functions
#' @param fun \code{character}, defines the function for the computation of the TIF, either by summing the items (sum) or by computing the mean (mean)
#' @param theta \code{numeric} latent trait level of person \eqn{p}, it can be a single value or a vector of values. Default is a vector of 1 thousand values ranging from -5 to +5
#'
#' @returns A \code{data.frame} of class \code{tif} with two columns: (i) \code{theta} containing the latent trait values, and (ii) \code{tif} containing the TIF values computed as either the sum or the mean of the IIFs
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
