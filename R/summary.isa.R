#' Method for the summary of the test/short test form
#'
#' The test/short test form is obtained with the ISA procedure implemented with function \code{isa()}. Details on the procedure can be found in the documentation of the \code{isa()} function.
#'
#' @param object Object of class \code{isa}
#' @param ... other arguments
#'
#' @returns A summary of the test obtained from the application of ISA
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
#' target <- tif(item_info(item_par), fun = "mean")
#' resI <- isa(item_par, target, nmin = 5)
#' summary(resI)
summary.isa<- function(object, ...) {
  cat("The item selection is based on the isa procedure requiring the selection of at least", unique(object$test$nmin),
      "items. \n")
  cat("The procedure resulted in the selection of the following", nrow(object$test))
  if (is.null(object$K)) {
    cat(" dichotomous items: \n")
  } else {
    cat(" of polytomous with", object$K+1, "categories \n")
  }
  cat(object$test$isel, "\nwith parameters: \n")
  print(object$selected_items)
}
