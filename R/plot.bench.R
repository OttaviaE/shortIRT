#' Method for plotting the TIF of the STF
#'
#' The STF is obtained with the bench procedure
#'
#' @param x Object of class theta_target
#' @param fun character, whether to consider the mean or the sum for the computation of the TIF
#' @param theta numeric, latent trait for the graphical representation
#' @param show_both logical, default is TRUE. Whether to show or not the TIF obtained from the full-length test
#' @param ... other arguments
#'
#' @returns A ggplot showing the TIFs of both the STF and the full-length test
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' theta <- rnorm(500, sd = 2)
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' resB <- bench(item_par, theta = theta, num_item = 5)
#' plot(resB)
#' # plot only the TIF of the STF
#' plot(resB, show_both = FALSE)
plot.bench <- function(x, fun = "sum",
                              theta = seq(-5, 5, length.out = 1000),
                             show_both = TRUE,
                              ...) {
  iif <- item_info(x$item_par, theta = theta)
  temp <- tif(iif, fun = fun, theta = theta)
  alltif <- data.frame(theta = temp$theta,
                       tif = temp$tif,
                       test = paste("all", nrow(x$item_par), "items"))
  stfiif <- item_info(x$selected_items, theta = theta)
  temp <- tif(stfiif, fun = fun, theta = theta)
  stftif <- data.frame(theta = temp$theta,
                       tif = temp$tif,
                       test = paste("stf with",
                                    nrow(x$selected_items), "items"))
  plottif <- rbind(alltif, stftif)
  if (show_both == TRUE) {
    basic_plot <-  ggplot(plottif,
                          aes(x = .data$theta, y = .data$tif,
                              group = .data$test, col = .data$test)) +
      geom_line()
  } else  {
    basic_plot <- ggplot(stftif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
      geom_line()
  }
  print(basic_plot)
}
