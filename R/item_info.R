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
