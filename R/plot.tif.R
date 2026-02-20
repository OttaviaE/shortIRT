#' Plot TIF
#'
#' @param x object of class \code{tif} obtained with the \code{tif()} function
#' @param ... other arguments
#'
#' @import ggplot2
#' @returns A \code{ggplot} displaying the TIF
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 5
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' iifs <- item_info(item_par)
#' test_tif <- tif(iifs)
#' plot(test_tif)
#' # compute the mean tif
#' test_tif_mean <- tif(iifs, fun = "mean")
#' plot(test_tif_mean)
plot.tif <- function(x, ...) {
  basic_plot <- ggplot2::ggplot(x,
                  ggplot2::aes(x = .data$theta, y = .data$tif)) +
    ggplot2::geom_line() + ggtitle(paste("TIF computed as",
                                         attributes(x)$source)) + theme_light()
  print(basic_plot)
}
