#' Item Information Function (single item, IIF)
#'
#' Compute the item information function (IIF) for a single dichotomous
#' or polytoumous item under either the 4-PL model (dichotomous item) or  a Generalized Partial Credit (polytomous item).
#' Specific models (e.g., 3-PL, 2-PL, 1-PL, or PCM, Rating Scale) are obtained by imposing
#' constraints on the item parameters.
#' See \code{Details}.
#'
#' @param item_pars Numeric matrix with one row corresponding to one item.
#'    For dichotomous items, the matrix must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the matrix has \eqn{2K} columns. The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters (must be named "b")
#'   \eqn{b_1, \dots, b_K}.
#' @param theta Numeric vector of latent trait values. Default is a vector of A thousand values ranging from -5 to +5
#' @param K Integer. Number of thresholds for  the categories of the polytoumous items (i.e., number of categories minus one). Default is \code{NULL} (assumes dichotomous items).
#' @details
#' Let \eqn{P(\theta)} denote the probability of a correct response under the
#' four-parameter logistic (4PL) model (dichotomous responses):
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
#' For a polytomous item with \eqn{K} thresholds seprating the \eqn{K + 1} categories, the probability of category \eqn{k} is defined as:
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
#' @returns A numeric vector of length equal to \code{theta}, which contains the item information function for a single item with respect to the values specified in \code{theta}
#' @export
#'
#' @examples
#'
#' theta <- seq(-4, 4, length.out = 200)
#'
#' # IIF of an item with b = 0
#' item_par <- data.frame(b = 0, a = 1.5, c = .10, e = .98)
#' info_dichotomous <- i_info(item_par, theta = theta)
#'
#' # 4-category item (K = 3)
#' item_pars <- data.frame(a1 = 1.2, a2 = 1.0, a3= 0.8,
#'                         b1 = -1.0, b2 = 0.0, b3 = 1.2)
#'
#' info <- iif_poly(item_pars, theta = theta, K = 3)
i_info <- function(item_pars,
                   theta = seq(-5,5,length.out=1000), K = NULL){
  if (is.null(K)) {
    if (ncol(item_pars) > 4) {
      stop("You forgot to specifiy the number of thresholds for your items!")
    }
    a <- item_pars$a
    e <- item_pars$e
    c <- item_pars$c
    P <- IRT(theta, b = item_pars$b, a = a, e = e, c=c)
    Q <- 1 - P
    num <- (a^2)*((P-c)^2)*((e-P)^2)
    den <- ((e-c)^2)*P*Q
    info <- num/den
  } else {
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
  }

  return(info)
}

#' Item Information Functions (multiple items, IIFs)
#'
#' Computes the item information functions for multiple dichotous or polytomous items
#'
#' @param item_pars \code{data.frame}, dataframe with nrows equal to the number of items. #'
#'    For dichotomous items, the matrix must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the matrix has \eqn{2K} columns. The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters (must be named "b")
#'   \eqn{b_1, \dots, b_K}.
#' @param theta Numeric vector of latent trait values. Default is a vector of A thousand values ranging from -5 to +5
#' @param K Integer. Number of thresholds for  the categories of the polytoumous items (i.e., number of categories minus one). Default is \code{NULL} (assumes dichotomous items).
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
#' # compute information for dichomtous items with default theta values
#' infos <- item_info(parameters)
#' head(infos)
#' # 4 items with 4 response categories (K = 3)
#' item_pars <- data.frame(matrix(c(
#'         1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
#'         0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
#'        0.5, 1.5, 1, -1.5, -1.0, 0,
#'        1, 1, 1, -1.5, -0, 0.5),
#'        nrow = 4,
#'        byrow = TRUE))
#' colnames(item_pars) <- paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
#' info_poly <- item_info(item_pars, K = 3)
#' head(info_poly)
item_info <- function(item_pars, theta = seq(-5,5,length.out=1000), K = NULL){
  if (is.null(K)) {
    if (ncol(item_pars) > 4) {
      stop("You forgot to specifiy the number of thresholds for your items!")
    }
  item <- lapply(1:nrow(item_pars), function(i) {
    i_info(item_pars[i, ],
           theta = theta
    )
  })
  item <- data.frame(do.call(cbind, item))
  if (length(unique(theta)) == 1) {
    theta <- paste(theta, 1:length(theta), sep = "-")
  }
  rownames(item) <- theta
  colnames(item) <- rownames(item_pars)
  } else {
    if (ncol(item_pars) != 2 * K)
      stop("item_pars must have exactly 2*K columns.")

    n_items <- nrow(item_pars)
    n_theta <- length(theta)

    info_mat <- matrix(NA_real_, n_theta, n_items)

    for (i in seq_len(n_items)) {
      info_mat[, i] <- i_poly(
        theta      = theta,
        item_pars = item_pars[i, , drop = FALSE],
        K         = K
      )
    }

    info_mat <- data.frame(info_mat)
    colnames(info_mat) <- rownames(item_pars)
    rownames(info_mat) <- theta

    item <- info_mat
  }
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
#' @param iifs object of class \code{iifs} containing the item information functions
#' @param fun \code{character}, defines the function for the computation of the TIF, either by summing the items (sum) or by computing the mean (mean)
#'
#' @returns A \code{data.frame} of class \code{tif} with two columns: (i) \code{theta} containing the latent trait values, and (ii) \code{tif} containing the TIF values computed as either the sum or the mean of the IIFs
#' @export
#'
#' @examples
#' set.seed(123)
#' theta <- rnorm(100)
#' n <- 5
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' iifs <- item_info(item_par)
#' test_tif <- tif(iifs)
tif <- function(iifs, fun = "sum") {
  if (inherits(iifs, "iifs") == FALSE) {
    stop("I need an object of class iifs")
  }
  theta <- as.numeric(rownames(iifs))
  tif_fun <- switch(fun,
                    sum = rowSums,
                    mean = rowMeans)
  thetif <- data.frame(theta,
                       tif = tif_fun(iifs))
  thetif <- structure(
    thetif,
    class = c("tif", class(thetif)),
    source = fun
  )

  return(thetif)
}
