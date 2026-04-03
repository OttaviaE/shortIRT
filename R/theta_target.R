#' Theta target procedure
#'
#' Develop a test or a short form given the parameters of dichotomous or polytomous in an item bank/full-length test according to the theta target procedure. See \code{Details}.
#'
#' @param targets \code{numeric}, either a vector with the discrete values of theta for which the information needs to be
#'    maximized obtained with the \code{define_targets()} function or a vector with user-defined values.
#'    If the same theta value is defined and repeated several time, it can be passed as a named list, where \code{value} indicate the value
#'    that needs to be repeated and \code{num_targets} the number of times it is repeated for.
#' @param item_pars \code{data.frame}, dataframe with nrows equal to the number of items.
#'    For dichotomous items, the matrix must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the matrix has \eqn{2K} columns, where \eqn{K} is the number of thresholds of the items (number of response categorie \eqn{- 1}). The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters (must be named "b")
#'   \eqn{b_1, \dots, b_K}.
#' @param theta \code{numeric} vector with the values of the latent trait \eqn{\theta} (needed for the computation of the IIFs of all items)
#' @param K \code{integer}, number of thresholds for  the categories of the polytoumous items (i.e., number of categories minus one). Default is \code{NULL} (assumes dichotomous items).
#' @details
#' A test or a short test form composed of \eqn{N} items is constructed with the aim of maximizing the information for \eqn{N} latent trait levels of interest, denoted as \eqn{\theta}-targets. An item for each \eqn{\theta}-target is chosen, hence the number of items included in the test is determined by the number of defined \eqn{\theta}-targets.
#' Let \eqn{B} be the set of items in the item bank and
#' \eqn{N} the desired number of \eqn{\theta}-targets and hence the desired number of items.
#' Let \eqn{t = 0, \dots, T} denote the iteration index of the procedure, with
#' \eqn{T = N - 1}.
#'
#' Define:
#' \itemize{
#'   \item \eqn{Q^t \subseteq \{1, \dots, B\}} as the set of items selected for
#'   inclusion in the short test form up to iteration \eqn{t};
#'
#'   \item \eqn{S^t \subseteq \{1, \dots, N\}} as the set of  \eqn{\theta}-targets
#'   satisfied up to iteration \eqn{t}.
#' }
#'
#' At \eqn{t = 0}, \eqn{Q^0 = \emptyset} and
#' \eqn{S^0 = \emptyset}.
#'
#' The procedure iterates the following steps until \eqn{t = T}:
#'
#' \enumerate{
#'   \item Select the item--target pair \eqn{(i, n)} maximizing the item
#'   information function:
#'
#'   \deqn{
#'   (i, n) =
#'   \arg\max_{i \in B \setminus S^t,\; n \in N \setminus Q^t}
#'   \mathrm{IIF}(i, n)
#'   }
#'
#'   \item Update the set of selected items:
#'   \deqn{
#'   Q^{t+1} = Q^t \cup \{i\}
#'   }
#'
#'   \item Update the set of satisfied ability targets:
#'   \deqn{
#'   S^{t+1} = S^t \cup \{n\}
#'   }
#' }
#'
#' At iteration \eqn{T}, the procedure yields
#' \eqn{|Q^{T+1}| = N} and \eqn{|S^{T+1}| = N}. Further details can be found in Epifania et al. (2022).
#'
#' @references Epifania, O. M., Anselmi, P., & Robusto, E. (2022). Item response
#' theory approaches for test shortening. In M. Wiberg, D. Molenaar,
#' J. Gonzalez, J. S. Kim, & H. Hwang (Eds.), Quantitative Psychology
#' (Vol. 422, pp. 75–83). Springer Proceedings in Mathematics and
#' Statistics. Springer, Cham.
#' https://doi.org/10.1007/978-3-031-27781-8_7
#'
#' @returns
#' An object of class \code{theta_target} of length 4 containing:
#'
#' \itemize{
#'   \item \strong{test}: a data frame containing the items selected for inclusion
#'   in the test (column \code{isel}), their maximum item information
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
#'
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
theta_target <- function(targets, item_pars, theta = seq(-5,5, length.out=1000),
                         K = NULL) {
  if (is.list(targets)) {
    if (length(targets) > 2) {
      stop("Only lists of length 2 are accepted with the specific theta value and the number of targets to generated")
    }
    if (targets$num_targets == nrow(item_pars)) {
      warning("The number of items is equal to the number of items in the item bank.")
    }
    if (targets$num_targets > nrow(item_pars)) {
      stop("The number of items of the test is greater than the number of items in the item bank")
    }
    targets <- rep(targets$value, targets$num_targets)
  }
  if (length(targets) == nrow(item_pars)) {
    warning("The number of items is equal to the number of items in the item bank.")
  }
  if (length(targets) > nrow(item_pars)) {
    stop("The number of items of the test is greater than the number of items in the item bank")
  }
 if (inherits(targets, "equal") == FALSE & inherits(targets, "clusters") == FALSE) {
    class_targets  <- "user-defined"
  } else {
    class_targets <- class(targets)
  }
  if (is.null(K) == FALSE) {
    itarget <- t(item_info(item_pars = item_pars,
                          theta = targets,
                          K = K))
    iifs <- item_info(item_pars, theta, K)
  } else {
    if (length(unique(gsub("[0-9]", "", colnames(item_pars)))) == 2) {
      stop("You forgot to specifiy the number of thresholds for your items!")
    } else {
      itarget <- t(item_info(item_pars = item_pars,
                             theta = targets))
      iifs <- item_info(item_pars, theta)
    }
  }

  colnames(itarget) <- paste("target", targets , sep ="")
  temp <- itarget

  isel <- numeric(length(targets))
  tsel <- numeric(length(targets))
  maxiif <- numeric(length(targets))
  for (i in 1:length(targets)) {
    isel[i] <- which(temp == max(temp, na.rm = TRUE), arr.ind = TRUE)[1,][1]
    tsel[i] <- which(temp == max(temp, na.rm = TRUE), arr.ind = TRUE)[1,][2]
    maxiif[i] <-  max(temp, na.rm = TRUE)
    temp[isel[i], ] <- NA
    temp[, tsel[i]] <- NA
  }
  rownames(iifs) <- theta
  res <- t(rbind(isel, maxiif))
  res <- data.frame(res)
  res$isel <-  as.character(rownames(item_pars)[isel])
  res$theta_target <- targets[tsel]
  sel_items <- item_pars[res$isel, ]
  results <- list(test = res,
                  item_pars  = item_pars,
                  selected_items = sel_items,
                  intervals = class_targets,
                  all_iifs = iifs,
                  K = K)
  class(results) <- "theta_target"
  return(results)
}
