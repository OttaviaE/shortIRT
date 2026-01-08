#' Method for the summary of the bench procedure
#'
#' @param object Object obtained from the application of theta target procedure with class theta_target
#' @param ... other arguments
#'
#' @returns A summary of the STF obatined from the application of the bench procedure
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
#' resB <- bench(item_par, theta = theta, num_item = 5)
#' summary(resB)
summary.bench <- function(object, ...) {
  cat("The selected items are\n")
  cat(object$stf$isel, "\n")
  cat("These items maximize the information for thetas equal to: \n")
  cat(object$stf$theta, "\n The parameters of the selected items are: \n")
  print(object$selected_items)
  cat("The item selection is based on the bench procedure \n")
}
