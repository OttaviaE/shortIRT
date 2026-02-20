#' Benchmark Procedure
#'
#' Create a Short Test Form (STF) using a benchmark procedure (i.e., the n most informative items are selected, where n is the number of items to include in the STF)
#'
#' @param item_pars \code{data.frame}, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#' @param iifs \code{data.frame}, dataframe with n-rows equal to the length of the latent trait \eqn{\theta} and n-cols equal to the number of items in the full-length test. It contains the item information functions (IIFs) of the items in the full-length test. Cannot use both \code{ipar} and \code{iifs}.
#' @param num_item \code{integer}, the number of items to include in the short test form
#' @param theta \code{numeric}, vector with the latent trait values
#' @param K Integer. Number of steps (categories minus one). Must be specified only if polytomous items are used.
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
#'   \item{\code{K}}: Number of thresholds for the response categories of the items. If the items are dichotomous, K = 1.
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
