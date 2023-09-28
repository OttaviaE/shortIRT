#' diff_theta
#'
#' Computes the difference between a starting value of theta and the theta estimated with the STF (view details)
#'
#' @param results The object obtained from the stf-generating functions
#' @param true_theta Default is NULL, optional vector with the true theta of the respondents
#'
#' @return A data frame with nrows equal to the number of respondents and 3 columns, one with the strating/true theta, one with the theta estimated with the STF, and the difference between the strtaing/true and the estimated one.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate person and item parameters
#' starting_theta = rnorm(100)
#' b <- runif(100, -3, 3)
#' a <- runif(100, 0.6, 2)
#' parameters <- data.frame(b, a)
#' # simulate data
#' data <- sirt::sim.raschtype(starting_theta, b = b, fixed.a = a)
#' stf <- uip(data, starting_theta = starting_theta, item_par = parameters, num_item = 5)
#' # without starting theta
#' my_diff <- diff_theta(stf)
#' head(my_diff)
#' }
diff_theta <- function(results, true_theta = NULL) {
  difference <- results$theta
  if (is.null(true_theta)) {
    lab <- "starting_theta"
  } else {
    if (length(true_theta) != nrow(difference)) {
      stop("True theta might have the same length as the estimated theta")
    }
    difference$starting_theta <- true_theta
    lab <- "true_theta"
  }
  difference$difference <- difference$starting_theta - difference$stf_theta
  difference$abs_difference <- abs(difference$difference)
  names(difference)[colnames(difference) == "starting_theta"] <- lab
  return(difference)
}
