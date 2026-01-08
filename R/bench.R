#' Benchmark Procedure
#'
#' Create a Short Test Form (STF) using a benchmark procedure (i.e., the n most informative items are selected, where n is the number of items to include in the STF)
#'
#' @param item_par data.frame, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective parameters.
#' @param iifs data.frame, dataframe with nrows equal to the lenght of theta and ncols equal to the number of items, it contains the IIFs of the items in the full-length test. Cannot use both ipar and iifs
#' @param num_item integer, the number of items to include in the short test form
#' @param theta numeric, vector with the latent trait values
#'
#' @returns
#' A list of length 3 with:
#'
#' - stf: dataframe with the items selected for inclusion in the STF (column isel), their maximum information function (maxiif), for  a specific theta (column theta)
#' - item_par: the original dataframe containing the item parameters
#' - selected_items: dataframe with the parameters of the selected items
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' theta <- rnorm(500, sd = 2)
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' resB <- bench(item_par, theta = theta, num_item = 5)
#' str(resB)
bench <- function(item_par = NULL,
                  iifs = NULL,
                  theta = NULL,
                  num_item = NULL) {
  if (is.null(num_item)) {
    stop("You must specify the number of items for the STFs!")
  }
  if (is.null(item_par) & is.null(iifs)) stop("You must specificy either the IIFs or the item parameters!")
  if (is.null(item_par) == FALSE & is.null(iifs) == TRUE) {
    iifs <- item_info(item_par, theta)
  } else if (is.null(item_par) == TRUE & is.null(iifs) == FALSE) {
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
  sel_items <- item_par[names(maxinfos), ]
  stf <- data.frame(isel = names(maxinfos),
                    maxiif = maxinfos,
                    theta = maxthetas[rownames(sel_items)])
  results <- list(stf = stf,
                  item_par = item_par,
                  selected_items = sel_items)
  class(results) <- "bench"
  return(results)
}
