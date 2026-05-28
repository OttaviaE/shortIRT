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
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Define the number of items in the item bank
#' n <- 50
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
#' # Compute item information functions and define
#' # a target Test Information Function (TIF)
#' # using the mean information across items
#' target <- tif(
#'   item_info(item_par),
#'   fun = "mean"
#' )
#'
#' # Run item selection algorithm (ISA)
#' # selecting at least 5 items
#' resI <- isa(
#'   item_par,
#'   target,
#'   nmin = 5
#' )
#'
#' # Summarize item selection results
#' summary(resI)
summary.isa<- function(object, ...) {
  if (is.null(object$K)) {
    type_items <- "dichotomous items: \n"
  } else {
    type_items <- paste("polytomous items with", object$K+1, "categories: \n")
  }
  if (nrow(object$test) == nrow(object$item_pars)) {
    cat("The item selection is based on the ISA. \nAll the", gsub(": \n", "", type_items), "from the item bank have been included in the test in the following order: \n")
  } else {
    cat("The item selection is based on the isa requiring the selection of at least", unique(object$test$nmin), "items",
        "\nThe procedure selected the following", nrow(object$test), type_items)
  }
  cat(object$test$isel, "\nwith parameters: \n")
  print(object$selected_items)
}
