#' Method for the summary of the theta target procedure
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
#' targets <- define_targets(theta, num_targets = 4)
#' resT <- theta_target(targets, item_par)
#' summary(resT)
summary.theta_target <- function(object, ...) {
  cat("The selected items are\n")
  cat(object$stf$isel, "\n")
  cat("These items maximize the information for thetas equal to: \n")
  cat(object$stf$theta_target, "\n with the following parameters \n")
  print(object$selected_items)
  cat("The item selection is based on the theta-target procedure with", object$intervals, "target")
}
