#' Compute expected probability
#'
#' Compute the expected probability for an item i according to its IRT parameters.
#' According to the parameters that are specified, the probability according to the 1-PL, 2-PL, 3-PL, or 4-PL models is computed
#'
#' @param theta latent trait level of person p, Can be a single value or a vector.
#' @param b location of item i. Default is 0.
#' @param a discrimination parameter for item i. Default is 1.
#' @param c pesudoguessing parameter of item i. Default is 0.
#' @param e Upper asymptote of item i. Default is 1.
#'
#' @returns a single value, that is the probability of the correct response for item i given the specified parameters
#' @export
#'
#' @examples
#' IRT(theta = 0, b = 0,  a = 1, c = 0, e = 1)
IRT <- function(theta,  b = 0, a = 1, c = 0,e = 1) {
  if (is.null(theta)) stop("Supply theta values")
  if (length(a) > 1 | length(b) > 1 | length(c)> 1 | length(e) > 1) stop("Compute the probability for a single item at the time")
  y <- c + (e - c) * (exp(a * (theta - b)) / (1 + exp(a * (theta - b))))
  return(y)
}
