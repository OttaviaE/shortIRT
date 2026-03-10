#' Compute expected probability for a single dichotomous item
#'
#' Compute the expected probability for an item \eqn{i} according to its IRT parameters.
#' Depending on the parameters that are specified, the probability is computed according to the 1-PL, 2-PL, 3-PL, or 4-PL models.
#'
#' @param theta \code{numeric} latent trait level of person \eqn{p}. It can be a single value or a vector of values.
#' @param b \code{numeric} location of item \eqn{i}. Default is 0.
#' @param a \code{numeric} discrimination parameter for item \eqn{i}. Default is 1.
#' @param c \code{numeric} pesudoguessing parameter of item \eqn{i}. Default is 0.
#' @param e \code{numeric} upper asymptote of item \eqn{i}. Default is 1.
#'
#' @details
#' The probability of a correct response \eqn{x_{pi} = 1} for person \eqn{p} on item \eqn{i} under the four-parameter logistic
#' (4-PL; Barton & Lord, 1981) model is defined as:
#'
#' \deqn{
#' P(x_{pi} = 1 \mid \theta_p, b_i, a_i, c_i, e_i) =
#' c_i + \frac{e_i - c_i}{1 + \exp\left[-a_i(\theta_p - b_i)\right]}
#' }
#'
#' where \eqn{a_i} is the discrimination parameter,
#' \eqn{b_i} is the difficulty parameter (or location of item \eqn{i} on the latent trait),
#' \eqn{c_i} is the lower asymptote (pseudo-guessing probability),
#' and \eqn{e_i} is the upper asymptote (inattention/slip). By constraining \eqn{e_i = 1}, \eqn{c_i = 0}, and \eqn{a_i=1} \eqn{\forall i}, the probability
#' is computed according to the 3-PL (Lord, 1980), 2-PL (Birnbaum, 1968) and 1-PL, respectively.
#'
#' @references Barton, M. A., & Lord, F. M. (1981). An upper asymptote for the
#' three-parameter logistic item-response model. ETS Research Report
#' Series, 1981(1), i–8. Princeton, NJ: Educational Testing Service.
#'
#' Birnbaum, A. (1968). Some latent trait models and their use in inferring
#' an examinee's ability. In F. M. Lord & M. R. Novick (Eds.),
#' Statistical theories of mental test scores (pp. 397–479).
#' Reading, MA: Addison-Wesley.
#'
#' Lord, F. M. (1980). Applications of item response theory to practical
#' testing problems. Hillsdale, NJ: Lawrence Erlbaum Associates.
#'
#' @returns a single value, that is the probability of the correct response for item \eqn{i} given the specified parameters
#' @export
#'
#' @examples
#' # compute the probability for an item according to 1-PL model
#' IRT(theta = 0, b = 0,  a = 1, c = 0, e = 1)
#' # compute probability for a vector of thetas for the same item
#' IRT(theta = c(-1, 0, 1), b = 0,  a = 1, c = 0, e = 1)
#' # compute probability for a vector of thetas for an item according to the 4-PL model
#' IRT(theta = c(-1, 0, 1), b = 0,  a = 1.25, c = 0.10, e = 0.98)
IRT <- function(theta,  b = 0, a = 1, c = 0,e = 1) {
  if (length(a) > 1 | length(b) > 1 | length(c)> 1 | length(e) > 1) stop("Compute the probability for a single item at the time")
  p <- c + (e - c)/(1 + exp((-a * (theta - b))))
  return(p)
}

#' Compute expected probability for multiple dichotomous items
#'
#' Compute the expected probability for multiple dichotomous items.
#' Depending on the parameters that are specified, the probability is computed according to the 1-PL, 2-PL, 3-PL, or 4-PL models.
#'
#' @param item_pars \code{data.frame}, dataframe with nrows equal to the number of items and 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location/difficulty \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#' @param theta \code{numeric} latent trait level of person \eqn{p}, it can be a single value or a vector of values.
#' @details
#' The probability of a correct response \eqn{x_{pi} = 1} for person \eqn{p} on item \eqn{i} under the four-parameter logistic
#' (4-PL; Barton & Lord, 1981) model is defined as:
#'
#' \deqn{
#' P(x_{pi} = 1 \mid \theta_p, b_i, a_i, c_i, e_i) =
#' c_i + \frac{e_i - c_i}{1 + \exp\left[-a_i(\theta_p - b_i)\right]}
#' }
#'
#' where \eqn{a_i} is the discrimination parameter,
#' \eqn{b_i} is the difficulty parameter (or location of item \eqn{i} on the latent trait),
#' \eqn{c_i} is the lower asymptote (pseudo-guessing probability),
#' and \eqn{e_i} is the upper asymptote (inattention/slip). By constraining \eqn{e_i = 1}, \eqn{c_i = 0}, and \eqn{a_i=1} \eqn{\forall i}, the probability
#' is computed according to the 3-PL (Lord, 1980), 2-PL (Birnbaum, 1968) and 1-PL, respectively.
#'
#' @references Barton, M. A., & Lord, F. M. (1981). An upper asymptote for the
#' three-parameter logistic item-response model. ETS Research Report
#' Series, 1981(1), i–8. Princeton, NJ: Educational Testing Service.
#'
#' Birnbaum, A. (1968). Some latent trait models and their use in inferring
#' an examinee's ability. In F. M. Lord & M. R. Novick (Eds.),
#' Statistical theories of mental test scores (pp. 397–479).
#' Reading, MA: Addison-Wesley.
#'
#' Lord, F. M. (1980). Applications of item response theory to practical
#' testing problems. Hillsdale, NJ: Lawrence Erlbaum Associates.
#'
#' @returns A \eqn{P \times I} matrix of class \code{mpirt} with the expected probability of observing a correct response for respondent \eqn{p} on item \eqn{i}
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
#' expected_prob <- mpirt(item_pars, theta)
mpirt <- function(item_pars, theta) {
  myp <- (matrix(nrow = length(theta),
                ncol = nrow(item_pars)))
  for(i in 1:nrow(item_pars)) {
    myp[,i] <- IRT(theta, b = item_pars$b[i],
                   a = item_pars$a[i],
                   c = item_pars$c[i],
                   e = item_pars$e[i])
  }
  myp <- structure(
    myp,
    class = c("mpirt", class(myp))
  )
  return(myp)
}

#' Simulate dichotomous responses according to IRT probabilities
#'
#' Simulate dichotomous responses according to IRT probabilities simulated with the \code{mpirt()} function .
#'
#' @param myp Object of class \code{mpirt} containing the expected IRT probabilities obtained with function \code{mpirt()}
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
#' expected_prob <- mpirt(item_pars, theta)
#' simulated_responses <- obsirt(expected_prob)
obsirt <- function(myp) {
  if (inherits(myp, "mpirt") == FALSE) {
    stop("Requires an object of class mpirt")
  }
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
