#' Theta target procedure
#'
#' Procedure based on the theta targets procedure for the generation of a short test form
#'
#' @param targets numeric vector with the discrete values of theta for which the information needs to be maximized
#' @param item_par dataframe, with nrows equals to the length of the latent trait and four columns, each denoting the IRT item parameters
#' @details
#' Let \eqn{k = 0, \dots, K} denote the iteration index of the procedure, with
#' \eqn{K = N - 1}. Let \eqn{J} be the total number of items in the item bank and
#' \eqn{N} the desired length of the short test form.
#'
#' Define:
#' \itemize{
#'   \item \eqn{S^k \subseteq \{1, \dots, J\}} as the set of items selected for
#'   inclusion in the short test form up to iteration \eqn{k};
#'
#'   \item \eqn{Q^k \subseteq \{1, \dots, N\}} as the set of ability targets
#'   satisfied up to iteration \eqn{k}.
#' }
#'
#' At initialization (\eqn{k = 0}), \eqn{S^0 = \varnothing} and
#' \eqn{Q^0 = \varnothing}.
#'
#' The procedure iterates the following steps until \eqn{k = K}:
#'
#' \enumerate{
#'   \item Select the item--target pair \eqn{(i, n)} maximizing the item
#'   information function:
#'
#'   \deqn{
#'   (i, n) =
#'   \arg\max_{i \in B \setminus S^k,\; n \in N \setminus Q^k}
#'   \mathrm{IIF}(i, n)
#'   }
#'
#'   \item Update the set of selected items:
#'   \deqn{
#'   S^{k+1} = S^k \cup \{i\}
#'   }
#'
#'   \item Update the set of satisfied ability targets:
#'   \deqn{
#'   Q^{k+1} = Q^k \cup \{n\}
#'   }
#' }
#'
#' At iteration \eqn{K}, the procedure yields
#' \eqn{|S^{K+1}| = N} and \eqn{|Q^{K+1}| = N}.
#'
#' @returns
#' An object of class \code{theta_target} of length 4 containing:
#'
#' \itemize{
#'   \item \strong{stf}: a data frame containing the items selected for inclusion
#'   in the short test form (column \code{isel}), their maximum item information
#'   function (column \code{maxiif}), and the corresponding theta target
#'   (column \code{theta_target}).
#'
#'   \item \strong{item_par}: the original data frame containing the item
#'   parameters.
#'
#'   \item \strong{selected_items}: a data frame containing the parameters of the
#'   selected items.
#'
#'   \item \strong{intervals}: a character string indicating how the theta
#'   targets were obtained. The value \code{"clusters"} denotes clustering of
#'   the latent trait, \code{"equal"} denotes equally spaced intervals, and
#'   \code{"unknown"} identifies any other case.
#' }
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' theta <- rnorm(500)
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' targets <- define_targets(theta, num_targets = 4)
#' resT <- theta_target(targets, item_par)
#' str(resT)
theta_target <- function(targets, item_par) {

  itarget = t(item_info(item_par = item_par,
                        theta = targets))
  if (inherits(targets, "equal") == FALSE & inherits(targets, "clusters") == FALSE) {
    class_targets  <- "unknown"
  } else {
    class_targets <- class(targets)
  }
  colnames(itarget) = paste("target", targets , sep ="")
  temp = itarget

  isel = numeric(length(targets))
  tsel = numeric(length(targets))
  maxiif = numeric(length(targets))

  for (i in 1:length(targets)) {
    isel[i] = which(temp == max(temp, na.rm = TRUE), arr.ind = TRUE)[1,][1]
    tsel[i] = which(temp == max(temp, na.rm = TRUE), arr.ind = TRUE)[1,][2]
    maxiif[i] =  max(temp, na.rm = TRUE)
    temp[isel[i], ] = NA
    temp[, tsel[i]] = NA
  }
  res <- t(rbind(isel, maxiif))
  res <- data.frame(res)
  res$isel <-  as.character(res$isel)
  res$theta_target <- targets[tsel]
  sel_items <- item_par[res$isel, ]
  results <- list(stf = res,
                  item_par  = item_par,
                  selected_items = sel_items,
                  intervals = class_targets)
  class(results) <- "theta_target"
  return(results)
}
