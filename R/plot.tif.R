#' Plot TIF
#'
#' Plot the test information function computed with the \code{tif()} function.
#'
#' @param x object of class \code{tif} obtained with the \code{tif()} function
#' @param ... other arguments
#'
#' @import ggplot2
#' @returns A \code{ggplot} displaying the TIF
#' @export
#'
#' @examples
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Define the number of items in the item bank
#' n <- 5
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
#' # Compute item information functions (IIFs)
#' iifs <- item_info(item_par)
#'
#' # Compute Test Information Function (TIF)
#' test_tif <- tif(iifs)
#'
#' # Plot the test information function
#' plot(test_tif)
#'
#' # Compute the mean TIF across items/components
#' test_tif_mean <- tif(iifs, fun = "mean")
#'
#' # Plot the mean test information function
#' plot(test_tif_mean)
plot.tif <- function(x, ...) {
  if (attributes(x)$source == "sum") {
    title <- "TIF"
  } else {
    title <- "Average TIF"
  }
  basic_plot <- ggplot2::ggplot(x,
                  ggplot2::aes(x = .data$theta, y = .data$tif)) +
    ggplot2::geom_line() + ggtitle(title) + theme_light()
  basic_plot
}
