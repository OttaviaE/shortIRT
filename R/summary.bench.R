#' Method for the summary of the test/short test form
#'
#' The test/short test form is obtained with the benchmark procedure implemented with function \code{bench()}. Details on the procedure can be found in the documentation of the \code{bench()} function.
#'
#' @param object Object of class \code{bench()}
#' @param ... other arguments
#'
#' @returns A summary of the test obtained from the application of the benchmark procedure
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
  if (is.null(object$K) & !is.null(object$selected_items)) {
    type_items <- "dichotomous items: \n"
  } else if (is.null(object$K) & !is.null(object$selected_items)) {
    type_items <- paste("polytomous items with", object$K+1, "categories: \n")
  } else {
    type_items <- "items: \n"
  }
  if (is.null(object$item_pars)) {
    placeholder <- ncol(object$all_iifs)
  } else {
    placeholder <- nrow(object$item_pars)
  }
  if (nrow(object$test) == placeholder) {
    cat("The item selection is based on the benchmark procedure. \nAll the", gsub(": \n", "", type_items), "from the item bank have been included in the test in the following order: \n")
  } else {
    cat("The item selection is based on the benchmark procedure. \nThe procedure selected the following", nrow(object$test), type_items)
  }
  cat(object$test$isel, "\n")
  if (!is.null(object$selected_items)) {
    cat("with parameters: \n")
    print(object$selected_items)
  }
  cat("These items maximize the information for thetas equal to: \n")
  cat(object$test$theta)
}
