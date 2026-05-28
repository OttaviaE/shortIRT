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
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Define the number of items in the item bank
#' n <- 50
#'
#' # Generate latent trait values for 500 respondents
#' theta <- rnorm(500)
#'
#' # Create item parameter matrix/data frame
#' # b = difficulty parameters
#' # a = discrimination parameters
#' # c = lower asymptote
#' # e = upper asymptote
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#'
#' # Define theta-based target regions
#' # splitting the latent trait distribution into 4 segments
#' targets <- define_targets(theta, num_targets = 4)
#'
#' # Perform theta-targeted item selection
#' # matching item information to target regions
#' resT <- theta_target(targets, item_par)
#'
#' # Summarize theta-targeted selection results
#' summary(resT)
summary.theta_target <- function(object, ...) {
  if (object$intervals == "clusters") {
    lab <- "cluster-defined"
  } else if (object$intervals == "equal") {
    lab <- "equally-spaced"
  } else {
    lab <- object$intervals
  }
  if (is.null(object$K)) {
    type_items <- "dichotomous items: \n"
  } else {
    type_items <- paste("polytomous items with", object$K+1, "categories: \n")
  }
  if (nrow(object$test) == nrow(object$item_pars)) {
    cat("The item selection is based on the theta-target procedure with",
        lab, "targets. \nAll the", gsub(": \n", "", type_items), "from the item bank have been included in the test in the following order: \n")
  } else {
    cat("The item selection is based on the theta-target procedure with", lab, "targets. \nThe procedure selected the following", nrow(object$test), type_items)
  }
  cat(object$test$isel, "\n")
  cat("with parameters: \n")
  print(object$selected_items)
  cat("These items maximize the information for thetas equal to: \n")
  cat(object$test$theta_target)
}
