#' Benchmark Procedure
#'
#' Develop a test or a short form of a test given the parameters of dichotomous or polytomous items in an item bank/full-length test according to the benchmark procedure. See \code{Details}.
#'
#' @param item_pars \code{data.frame}, dataframe with number of rows equal to the number of items.
#'    For dichotomous items, the dataframe must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the dataframe has \eqn{2K} columns, where \eqn{K} is the number of thresholds of the items (number of response categorie \eqn{- 1}). The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters
#'   \eqn{b_1, \dots, b_K} (must be named "b").
#' @param iifs \code{data.frame}, dataframe with number of rows equal to the length of the latent trait \eqn{\theta} and number of columns equal to the number of items in the item bank. It contains the item information functions (IIFs) of the items in item bank/the full-length test. The arguments \code{item_pars} and \code{iifs} cannot be used together.
#' @param num_item \code{integer}, the number \eqn{N} of items to include in the test.
#' @param theta \code{numeric}, vector with the latent trait values.
#' @param K \code{integer}, number of thresholds for  the categories of the polytoumous items (i.e., number of response categories minus 1). Default is \code{NULL} (assumes dichotomous items).
#'
#' @details
#' A test composed of \eqn{N} items is constructed from an item bank
#' \eqn{B} by selecting the items with the highest item information values, with no
#' explicit reference to any specific level of the latent trait. The same procedure can be applied
#' to obtain a short test form from a full-length test \eqn{B}.
#'
#' Let \eqn{I_i(\theta)} denote the item information function (IIF) of item
#' \eqn{i}, with \eqn{i = 1, \dots, |B|}, where \eqn{|B|} denotes the
#' cardinality of the item bank \eqn{B}.
#'
#' For each item \eqn{i}, compute the maximum value of its information
#' function over \eqn{\theta}. Define the vector
#'
#' \deqn{
#' \mathbf{m} = (m_1, \dots, m_{|B|}),
#' }
#'
#' where
#'
#' \deqn{
#' m_i = \max_{\theta} I_i(\theta), \qquad i = 1, \dots, |B|.
#' }
#'
#' The vector \eqn{\mathbf{m}} is then sorted in decreasing order, and the first \eqn{N} items in the ordered vector (i.e., the items with the highest information functions), with
#' \eqn{N < |B|}, are selected to form the test.
#'
#' Further details on the benchmark procedure can be found in Epifania et al. (2022).
#'
#' @references Epifania, O. M., Anselmi, P., & Robusto, E. (2022). Item response
#' theory approaches for test shortening. In M. Wiberg, D. Molenaar,
#' J. Gonzalez, J. S. Kim, & H. Hwang (Eds.), Quantitative Psychology
#' (Vol. 422, pp. 75–83). Springer Proceedings in Mathematics and
#' Statistics. Springer, Cham.
#' https://doi.org/10.1007/978-3-031-27781-8_7
#'
#' @returns
#' An object of class \code{bench} of length 3 with:
#'
#' \itemize{
#'   \item{\code{test}: dataframe with the items selected for inclusion in the test (\code{isel}),
#'   their maximum information function (\code{maxiif}), for a specific latent trait
#'   level \eqn{\theta} (column \code{theta}).}
#'   \item{\code{item_pars}: the original dataframe containing the item parameters.}
#'   \item{\code{selected_items}: dataframe with the parameters of the selected items.}
#'   \item{\code{K}: number of thresholds for the response categories of the items. If the items are dichotomous \code{K} is \code{NULL}.}
#' }
#'
#' @export
#'
#' @examples
#' # set a seed for the reproducibility of the results
#' set.seed(123)
#' # define the number of items in the item bank
#' n <- 50
#' # generate 500 random values of theta from a normal distribution with sd = 2
#' theta <- rnorm(500, sd = 2)
#' # generate item parameters  of the items in the item bank according to the 2-PL model
#' item_pars <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' # apply benchmark procedures
#' resB <- bench(item_pars, theta = theta, num_item = 5)
#' str(resB)
#' # generate an item bank with 4 polytomous items with K = 3
#' item_pars <- data.frame(matrix(c(
#'  1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
#'  0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
#'  0.5, 1.5, 1, -1.5, -1.0, 0,
#'  1, 1, 1, -1.5, -0, 0.5
#'  ), nrow = 4, byrow = TRUE))
#' # rename the columns
#' colnames(item_pars) = paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
#' resB_poly <- bench(item_pars, theta = theta, num_item = 2, K = 3)
#' str(resB_poly)
bench <- function(item_pars = NULL,
                  iifs = NULL,
                  theta = NULL,
                  num_item = NULL,
                  K = NULL) {
  if (is.null(num_item)) {
    stop("You must specify the number of items for the test!")
  }
  if (is.null(item_pars) & is.null(iifs)) stop("You must specificy either the IIFs or the item parameters!")
  if (is.null(item_pars) == FALSE & is.null(iifs) == TRUE) {
    if (num_item == nrow(item_pars)) {
      warning("The number of items is equal to the number of items in the item bank.")
    }
    if (num_item > nrow(item_pars)) {
      stop("The number of items of the test is greater than the number of items in the item bank")
    }
    if (!is.null(K)) {
      iifs <- item_info(item_pars = item_pars,theta = theta,  K = K)
    } else {
      if (length(unique(gsub("[0-9]", "", colnames(item_pars)))) == 2) {
        stop("You forgot to specifiy the number of thresholds for your items!")
      } else {
        iifs <- item_info(item_pars = item_pars,theta = theta,)
      }
    }
    rownames(iifs) <- theta
  } else if (is.null(item_pars) == TRUE & is.null(iifs) == FALSE) {
    if (num_item == ncol(iifs)) {
      warning("The number of items is equal to the number of items in the item bank.")
    }
    if (num_item > ncol(iifs)) {
      stop("The number of items of the test is greater than the number of items in the item bank")
    }
    iifs <- iifs
  } else {
    stop("Too much")
  }
  maxinfos <- apply(iifs, 2, max)
  maxinfos <- maxinfos[order(maxinfos, decreasing = TRUE)]
  maxinfos <- maxinfos[1:num_item]
  maxthetas <- round(as.numeric(rownames(iifs)[apply(iifs, 2, which.max)]),3)
  names(maxthetas) <- colnames(iifs)
  sel_items <- item_pars[names(maxinfos), ]
  stf <- data.frame(isel = names(maxinfos),
                    maxiif = maxinfos,
                    theta = maxthetas[names(maxinfos)])
  results <- list(test = stf,
                  item_pars = item_pars,
                  selected_items = sel_items,
                  all_iifs = iifs,
                  K = K)
  class(results) <- "bench"
  return(results)
}
