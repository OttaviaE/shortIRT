#' Method for the summary of the test/short test form
#'
#' The test/short test form is obtained with the theta target procedure implemented with function \code{theta_target()}. Details on the procedure can be found in the documentation of the \code{theta_target()} function.
#'
#' @param object Object of class \code{theta_target}
#' @param ... other arguments
#'
#' @returns A summary of the test obtained from the application of the theta target procedure
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
  if (object$intervals == "clusters") {
    lab <- "cluster-defined"
  } else if (object$intervals == "equal") {
    lab <- "equally-spaced"
  } else {
    lab <- object$intervals
  }
  cat("The item selection is based on the theta-target procedure with", lab, "targets. \nThe procedure selected the following", nrow(object$test))
  if (is.null(object$K)) {
    cat(" dichotomous items: \n")
  } else {
    cat(" polytomous items with", object$K+1, "categories: \n")
  }
  cat(object$stf$isel, "\n")
  cat("with parameters: \n")
  print(object$selected_items)
  cat("These items maximize the information for thetas equal to: \n")
  cat(object$stf$theta_target)
}
