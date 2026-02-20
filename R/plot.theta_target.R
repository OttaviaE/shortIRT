#' Method for plotting the TIF of the STF
#'
#' The STF is obtained with the theta target procedure
#'
#' @param x Object of class \code{theta_target} obtained with function \code{theta_target()}
#' @param fun \code{character}, whether to consider the mean or the sum for the computation of the TIF
#' @param show_targets \code{logical}, default is TRUE. Whether to show or not the theta targets. If \code{TRUE} the theta targets are shown as the items that satisfy them.
#' @param show_both \code{logical}, default is \code{TRUE}. Whether to show or not the TIF obtained from the full-length test
#' @param ... other arguments
#'
#' @returns A \code{ggplot} showing the TIFs of both the STF and the full-length test
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
                              show_targets = TRUE, show_both = TRUE, K = NULL,
                              ...) {
  theta <- as.numeric(rownames(x$all_iifs))
  K <- x$K
  temp <- tif(x$all_iifs, fun = fun)
  alltif <- data.frame(theta = temp$theta,
                       tif = temp$tif,
                       test = paste("all", nrow(x$item_par), "items"))
  if (is.null(K)) {
    if (ncol(x$item_par) > 4) {
      stop("The items appear to be polytomous but you did not provide the K thresholds!")
    } else {
      stfiif <- item_info(x$selected_items, theta = theta)
    }
  } else {
    stfiif <- item_info(x$selected_items, theta = theta, K = K)
  }
  temp <- tif(stfiif, fun = fun)
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
    geom_line() + theme_light()
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
