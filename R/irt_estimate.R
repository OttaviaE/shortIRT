#' Log-likelihood estiamtion of theta
#'
#' @param theta \code{numeric} vector with true values of \eqn{\theta}
#' @param x \code{integer} vector of 0s and 1s, response pattern of each respondent
#' @param item_par \code{data.frame}, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'
#' @importFrom stats optim rbinom
#'
#' @returns The log-likelihood
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
#' obs_response <- obsirt(mpirt(item_par, theta))
#' # LogLikelihood of theta
#' logLik_theta(theta, obs_response, item_par)
logLik_theta <- function(theta, x, item_par) {

  P <- mpirt(item_par,
             theta = theta
  )
  sum(x * log(P) + (1 - x) * log(1 - P))
}

#' Estimate of theta
#'
#' Maximum Likelihood estimation of theta
#'
#' @param item_par \code{data.frame}, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#' @param responses \code{matrix}, \eqn{p \times i} matrix with the dichotomous responses of each respondent \eqn{p} on each item \eqn{i}. Default is \code{NULL}.
#' @param theta \code{numeric} latent trait level of person \eqn{p}, it can be a single value or a vector of values.
#' @param lower \code{integer} lower value of \eqn{\theta} to be considered for the estimation
#' @param upper \code{integer} upper value of \eqn{\theta} to be considered for the estimation
#'
#' @returns A numeric vector of length equal to the length of \code{theta} with the ML estimation of the latent trait
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
#' # estimate theta
#' theta_hat <- irt_estimate(item_par, theta = theta)
#' plot(theta, theta_hat)
irt_estimate <- function(item_par, responses = NULL,
                         theta, lower = -3, upper = abs(lower)) {
  myp <- mpirt(item_par, theta)
  if (is.null(responses)) {
    risp <- obsirt(myp)
  } else {
    risp <- responses
  }
  theta_hat <- apply(risp, 1, function(xj) {
    optim(
      par = 0,
      lower = lower,
      upper = upper,
      fn = logLik_theta,
      x = xj,
      item_par = item_par,
      control = list(fnscale = -1),
      method = "Brent"
    )$par
  })
  return(theta_hat)
}
