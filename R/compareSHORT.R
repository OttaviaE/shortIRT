#' Compare short forms obtained with different strategies
#'
#'
#' @param a vector of discrimination parameters
#' @param b vector of difficulty parameters
#' @param true_theta respondents' theta
#' @param theta_target number of items for the short form (it defines the theta target for the \code{cluster} and \code{guided} strategies)
#'
#' @return a list
#' @export
#'
#' @examples
compareSHORT = function(a = stats::runif(10, .40, 2),
                        b = stats::runif(10, -3, 3),
                        true_theta = stats::rnorm(1000),
                        theta_target = 2) {

  results = list()
  strategy = c("smart", "cluster", "guided")

  for (i in 1:length(strategy)) {
    results[[i]] = theoryIRT(a, b, true_theta,
                             theta_target = theta_target,
                             strategy = strategy[i])
    names(results)[i] = strategy[i]
  }
  class(results) = append(class(results), "compareSHORT")
  return(results)

}
