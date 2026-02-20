#' Benchmark Procedure
#'
#' Develop a short test form given the item parameters (dichotomous or polytomous) according to the benchmark procedure. See \code{Details}.
#'
#' @param item_pars \code{data.frame}, dataframe with nrows equal to the number of items. #'
#'    For dichotomous items, the matrix must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the matrix has \eqn{2K} columns. The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters (must be named "b")
#'   \eqn{b_1, \dots, b_K}.
#' @param iifs \code{data.frame}, dataframe with n-rows equal to the length of the latent trait \eqn{\theta} and n-cols equal to the number of items in the full-length test. It contains the item information functions (IIFs) of the items in the full-length test. Cannot use both \code{ipar} and \code{iifs}.
#' @param num_item \code{integer}, the number of items to include in the short test form
#' @param theta \code{numeric}, vector with the latent trait values
#' @param K Integer. Number of thresholds for  the categories of the polytoumous items (i.e., number of categories minus one). Default is \code{NULL} (assumes dichotomous items).
#'
#' @details
#' A short test form composed of \eqn{N} items is constructed from an item bank
#' \eqn{B} by selecting the items with the highest item information values.
#'
#' Let \eqn{I_i(\theta)} denote the item information function (IIF) for item
#' \eqn{i}, with \eqn{i = 1, \dots, |B|}. The IIFs of the item bank are sorted in
#' decreasing order:
#'
#' \deqn{
#' \mathrm{iif} =
#' \left(
#' \max_{1 \le i \le |B|} I_i(\theta),
#' \dots,
#' \min_{1 \le i \le |B|} I_i(\theta)
#' \right)
#' }
#'
#' The first \eqn{N} items in the ordered vector \eqn{\mathrm{iif}}, with
#' \eqn{N < |B|}, are selected to be included in the short test form.
#' @returns
#' An object of class \code{bench} of length 3 with:
#'
#' \itemize{
#'   \item{\code{stf}: dataframe with the items selected for inclusion in the STF (\code{isel}),
#'   their maximum information function (\code{maxiif}), for a specific latent trait
#'   level \eqn{\theta} (column \code{theta})}
#'   \item{\code{item_pars}: the original dataframe containing the item parameters}
#'   \item{\code{selected_items}: dataframe with the parameters of the selected items}
#'   \item{\code{K}: Number of thresholds for the response categories of the items. If the items are dichotomous \code{K} is \code{NULL}.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' theta <- rnorm(500, sd = 2)
#' item_pars <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' resB <- bench(item_pars, theta = theta, num_item = 5)
#' str(resB)
#' # same but with polytomous items
#' item_pars <- data.frame(matrix(c(
#'  1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
#'  0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
#'  0.5, 1.5, 1, -1.5, -1.0, 0,
#'  1, 1, 1, -1.5, -0, 0.5
#'  ), nrow = 4, byrow = TRUE))
#' colnames(item_pars) = paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
#' resB_poly <- bench(item_pars, theta = theta, num_item = 2, K = 3)
#' str(resB_poly)
bench <- function(item_pars = NULL,
                  iifs = NULL,
                  theta = NULL,
                  num_item = NULL,
                  K = NULL) {
  if (is.null(num_item)) {
    stop("You must specify the number of items for the STFs!")
  }
  if (is.null(item_pars) & is.null(iifs)) stop("You must specificy either the IIFs or the item parameters!")
  if (is.null(item_pars) == FALSE & is.null(iifs) == TRUE) {
    if (!is.null(K)) {
      iifs <- item_info(item_pars = item_pars,theta = theta,  K = K)
    } else {
      if (ncol(item_pars) > 4) {
        stop("You provided parameters for polytomous items but did not specified the number of thresholds")
      } else {
        iifs <- item_info(item_pars = item_pars,theta = theta,)
      }
    }
  } else if (is.null(item_pars) == TRUE & is.null(iifs) == FALSE) {
    iifs <- iifs
  } else {
    stop("Too much")
  }
  rownames(iifs) <- theta
  maxinfos <- apply(iifs, 2, max)
  maxinfos <- maxinfos[order(maxinfos, decreasing = TRUE)]
  maxinfos <- maxinfos[1:num_item]
  maxthetas <- round(as.numeric(rownames(iifs)[apply(iifs, 2, which.max)]),3)
  names(maxthetas) <- colnames(iifs)
  sel_items <- item_pars[names(maxinfos), ]
  stf <- data.frame(isel = names(maxinfos),
                    maxiif = maxinfos,
                    theta = maxthetas[rownames(sel_items)])
  results <- list(stf = stf,
                  item_pars = item_pars,
                  selected_items = sel_items,
                  all_iifs = iifs,
                  K = K)
  class(results) <- "bench"
  return(results)
}
