#' Method for plotting the TIF of the STF
#'
#' The STF is obtained with the theta target procedure
#'
#' @param x Object of class theta_target
#' @param fun character, whether to consider the mean or the sum for the computation of the TIF
#' @param theta numeric, latent trait for the graphical representation
#' @param show_targets logical, default is TRUE. Whether to show or not the theta targets
#' @param show_both logical, default is TRUE. Whether to show or not the TIF obtained from the full-length test
#' @param ... other arguments
#'
#' @returns A ggplot showing the TIFs of both the STF and the full-length test
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
#' plot(resT)
#' # plot without showing the theta targets
#' plot(resT, show_targets = FALSE)
plot.theta_target <- function(x, fun = "sum",
                              theta = seq(-5, 5, length.out = 1000),
                              show_targets = TRUE, show_both = TRUE,
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
  tt <- x$stf
  colnames(tt)[3] <- "theta"

  basic_plot <-   ggplot(plottif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
    geom_line()
  if (show_both == TRUE & show_targets == TRUE) {
    basic_plot <- basic_plot + geom_point(data = tt,
                                              aes(x = .data$theta,
                                                  y = min(plottif$tif) + .10,
                                                  pch = .data$isel),
                                          inherit.aes = FALSE)
  } else if (show_both == TRUE & show_targets == FALSE) {
    basic_plot <- basic_plot
  } else if (show_both == FALSE & show_targets == TRUE) {
    basic_plot <- ggplot(stftif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
      geom_line() + geom_point(data = tt,
                               aes(x = .data$theta, y = min(plottif$tif) + .10,
                                   pch = .data$isel),  inherit.aes = FALSE)
  } else {
    basic_plot <- ggplot(stftif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test,
                             col = .data$test)) + geom_line()
  }

 print(basic_plot)
}
