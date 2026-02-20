#' Theta target procedure
#'
#' Develop a short test form given the item parameters (dichotomous or polytomous) according to the theta target procedure. See \code{Details}.
#'
#' @param targets numeric vector with the discrete values of theta for which the information needs to be maximized
#' @param item_pars \code{data.frame}, dataframe with nrows equal to the number of items. #'
#'    For dichotomous items, the matrix must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the matrix has \eqn{2K} columns. The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters (must be named "b")
#'   \eqn{b_1, \dots, b_K}.
#' @param K Integer. Number of thresholds for  the categories of the polytoumous items (i.e., number of categories minus one). Default is \code{NULL} (assumes dichotomous items).
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
#'   \item \strong{item_pars}: the original data frame containing the item
#'   parameters.
#'
#'   \item \strong{selected_items}: a data frame containing the parameters of the
#'   selected items.
#'
#'   \item \strong{intervals}: a character string indicating how the theta
#'   targets were obtained. The value \code{"clusters"} denotes clustering of
#'   the latent trait, \code{"equal"} denotes equally spaced intervals, and
#'   \code{"unknown"} identifies any other case (e.g., user-defined theta targets).
#'
#'   \item{\code{K}: Number of thresholds for the response categories of the items. If the items are dichotomous \code{K} is \code{NULL}.}
#' }
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' theta <- rnorm(500)
#' item_pars <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' targets <- define_targets(theta, num_targets = 4)
#' resT <- theta_target(targets, item_pars)
#' str(resT)
#' # polytomous items with user defined theta targets
#' item_pars <- data.frame(matrix(c(
#'  1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
#'  0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
#'  0.5, 1.5, 1, -1.5, -1.0, 0,
#'  1, 1, 1, -1.5, -0, 0.5
#'  ), nrow = 4, byrow = TRUE))
#' colnames(item_pars) = paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
#' resT_poly <- theta_target(c(-1,0), item_pars, K = 3)
#' str(resT_poly)
theta_target <- function(targets, item_pars, K = NULL) {
 if (inherits(targets, "equal") == FALSE & inherits(targets, "clusters") == FALSE) {
    class_targets  <- "unknown"
  } else {
    class_targets <- class(targets)
  }
  if (is.null(K) == FALSE) {
    itarget <- t(item_info(item_pars = item_pars,
                          theta = targets,
                          K = K))
    iifs <- item_info(item_pars, theta, K)
  } else {
    if (ncol(item_pars) > 4) {
      stop("You provided parameters for polytomous items but did not specified the number of thresholds")
    } else {
      itarget <- t(item_info(item_pars = item_pars,
                             theta = targets))
      iifs <- item_info(item_pars, theta)
    }
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
  rownames(iifs) <- theta
  res <- t(rbind(isel, maxiif))
  res <- data.frame(res)
  res$isel <-  as.character(rownames(item_pars)[isel])
  res$theta_target <- targets[tsel]
  sel_items <- item_pars[res$isel, ]
  results <- list(stf = res,
                  item_pars  = item_pars,
                  selected_items = sel_items,
                  intervals = class_targets,
                  all_iifs = iifs,
                  K = K)
  class(results) <- "theta_target"
  return(results)
}
