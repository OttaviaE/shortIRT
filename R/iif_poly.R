#' Item Information Function for Polytomous IRT Models (GPCM/NRM-style)
#'
#' Computes the Item Information Function (IIF) for a single polytomous item
#' under a Generalized Partial Credit / Nominal Response formulation.
#' Specific models (e.g., PCM, Rating Scale) are obtained by imposing
#' constraints on the item parameters.
#'
#' @param theta Numeric vector of latent trait values.
#' @param item_pars Numeric matrix of item parameters with dimension
#'   \eqn{1 \times 2K}. The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K}, and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters
#'   \eqn{b_1, \dots, b_K}.
#' @param K Integer. Number of steps (i.e., number of categories minus one).
#'
#' @details
#' The probability of category \eqn{k} is defined as:
#' \deqn{
#' P(Y = k \mid \theta) =
#' \frac{\exp\left( \sum_{s=1}^k a_s (\theta - b_s) \right)}
#' {\sum_{j=0}^K \exp\left( \sum_{s=1}^j a_s (\theta - b_s) \right)}
#' }
#'
#' with the convention that the empty sum (for \eqn{k = 0}) is equal to zero.
#'
#' The item information is computed as:
#' \deqn{
#' I(\theta) = \sum_{k=0}^K \frac{[P'_k(\theta)]^2}{P_k(\theta)}
#' }
#'
#' @return A numeric vector of the same length as \code{theta} containing
#'   the item information values.
#'
#' @seealso \code{\link[mirt]{iteminfo}}
#'
#' @examples
#' theta <- seq(-4, 4, length.out = 200)
#'
#' # 4-category item (K = 3)
#' item_pars <- matrix(
#'   c(1.2, 1.0, 0.8,  -1.0, 0.0, 1.2),
#'   nrow = 1
#' )
#'
#' info <- iif_poly(theta, item_pars, K = 3)
#' plot(theta, info, type = "l",
#'      xlab = expression(theta),
#'      ylab = "Item Information")
#'
#' @export
i_poly <- function(theta, item_pars, K) {

  if (!is.numeric(theta))
    stop("theta must be a numeric vector.")

  # if (!is.matrix(item_pars) || nrow(item_pars) != 1)
  #   stop("item_pars must be a numeric matrix with one row.")

  if (ncol(item_pars) != 2 * K)
    stop("item_pars must have exactly 2*K columns.")

  # extract parameters
  a <- as.numeric(item_pars[1, 1:K])
  b <- as.numeric(item_pars[1, (K + 1):(2 * K)])

  n_theta <- length(theta)

  # linear predictors for categories 0,...,K
  eta <- sapply(0:K, function(k) {
    if (k == 0) {
      rep(0, n_theta)
    } else {
      rowSums(
        sapply(1:k, function(s) a[s] * (theta - b[s]))
      )
    }
  })

  # category probabilities
  exp_eta <- exp(eta)
  P <- exp_eta / rowSums(exp_eta)

  # derivatives of eta with respect to theta
  deta <- sapply(0:K, function(k) {
    if (k == 0) {
      rep(0, n_theta)
    } else {
      rep(sum(a[1:k]), n_theta)
    }
  })

  # derivatives of probabilities
  dP <- P * (deta - rowSums(P * deta))

  # item information
  info <- rowSums((dP^2) / P, na.rm = TRUE)

  return(info)
}



#' Item Information Matrix for Polytomous IRT Models
#'
#' Computes the Item Information Function (IIF) for multiple polytomous items
#' under a GPCM/NRM-style parametrization.
#'
#' @param item_par Numeric matrix of item parameters with dimension
#'   \eqn{n_{item} \times 2K}. Columns must be ordered as
#'   \eqn{a_1,\dots,a_K,b_1,\dots,b_K}.
#' @param theta Numeric vector of latent trait values.
#' @param K Integer. Number of steps (categories minus one).
#'
#' @return A numeric matrix with dimension
#'   \eqn{length(theta) \times n_{item}}. Each column corresponds
#'   to the item information curve of one item.
#'
#' @export
iif_poly <- function(item_par, theta, K) {

  if(!is.data.frame(item_par)) {
    stop("Item par must be a dataframe")
  }

  if (ncol(item_par) != 2 * K)
    stop("item_par must have exactly 2*K columns.")

  n_items <- nrow(item_par)
  n_theta <- length(theta)

  info_mat <- matrix(NA_real_, n_theta, n_items)

  for (i in seq_len(n_items)) {
    info_mat[, i] <- i_poly(
      theta      = theta,
      item_par = item_par[i, , drop = FALSE],
      K         = K
    )
  }

  info_mat <- data.frame(info_mat)
  colnames(info_mat) <- rownames(item_par)
  rownames(info_mat) <- theta

  info_mat <- structure(
    info_mat,
    class = c("iifs", class(info_mat))
  )

  return(info_mat)
}
