#' Compute expected probability for a single item
#'
#' Compute the expected probability for an item \eqn{i} according to its IRT parameters.
#' According to the parameters that are specified, the probability according to the 1-PL, 2-PL, 3-PL, or 4-PL models is computed
#'
#' @param theta \code{numeric} latent trait level of person p. It can be a single value or a vector of values.
#' @param b \code{numeric} location of item \eqn{i}. Default is 0.
#' @param a \code{numeric} discrimination parameter for item \eqn{i}. Default is 1.
#' @param c \code{numeric} pesudoguessing parameter of item \eqn{i}. Default is 0.
#' @param e \code{numeric} upper asymptote of item \eqn{i}. Default is 1.
#'
#' @details
#' The probability of a correct response under the four-parameter logistic
#' (4PL) model is defined as:
#'
#' \deqn{
#' P(X = 1 \mid \theta_p) =
#' c_i + \frac{e_i - c_i}{1 + \exp\left[-a_i(\theta_p - b_i)\right]}
#' }
#'
#' where \eqn{a} is the discrimination parameter,
#' \eqn{b} is the difficulty parameter,
#' \eqn{c} is the lower asymptote (guessing),
#' and \eqn{e} is the upper asymptote (inattention/slip). By constraining \eqn{e_i = 1}, \eqn{c_i = 0}, and \eqn{a_i=1} \eqn{\forall i}, the probability
#' is computed according to the 3-PL, 2-PL and 1-PL, respectively
#'
#' @returns a single value, that is the probability of the correct response for item \eqn{i} given the specified parameters
#' @export
#'
#' @examples
#' IRT(theta = 0, b = 0,  a = 1, c = 0, e = 1)
#' # compute probability for a vector of thetas for the same item
#' IRT(theta = c(-1, 0, 1), b = 0,  a = 1, c = 0, e = 1)
IRT <- function(theta,  b = 0, a = 1, c = 0,e = 1) {
  if (length(a) > 1 | length(b) > 1 | length(c)> 1 | length(e) > 1) stop("Compute the probability for a single item at the time")
  p <- c + (e - c)/(1 + exp((-a * (theta - b))))
  return(p)
}

#' Compute expected probability for multiple items
#'
#' @param item_par \code{data.frame}, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#' @param theta \code{numeric} latent trait level of person \eqn{p}, it can be a single value or a vector of values.
#'
#' @returns A \eqn{p \times i} matrix of class \code{mpirt} with the expected probability of observing a correct response for respondent \eqn{p} on item \eqn{i}
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
#' expected_prob <- mpirt(item_par, theta)
mpirt <- function(item_par, theta) {
  myp <- (matrix(nrow = length(theta),
                ncol = nrow(item_par)))
  for(i in 1:nrow(item_par)) {
    myp[,i] <- IRT(theta, b = item_par$b[i],
                   a = item_par$a[i],
                   c = item_par$c[i],
                   e = item_par$e[i])
  }
  myp <- structure(
    myp,
    class = c("mpirt", class(myp))
  )
  return(myp)
}

#' Simulate responses according to IRT probabilities
#'
#' @param myp Object of class \code{mpirt} containing the expected IRT probabilities obtained with function \code{mpirt()}
#'
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
#' expected_prob <- mpirt(item_par, theta)
#' simulated_responses <- obsirt(expected_prob)
obsirt <- function(myp) {
  # chatgpt
  n <- nrow(myp)
  k <- ncol(myp)

  Y <- matrix(
    rbinom(n * k, size = 1,
           prob = as.vector(myp)),
    nrow = n,
    ncol = k
  )
  Y <- structure(
    Y,
    class = c("obsirt", class(Y))
  )
  return(Y)
}
