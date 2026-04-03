#' Method for plotting the TIF of the test/short test form
#'
#' The test/short test form is obtained with the ISA procedure implemented with function \code{isa()}. Details on the procedure can be found in the documentation of the \code{isa()} function.
#'
#' @param x Object of class \code{isa} obtained with function \code{isa()}
#' @param fun \code{character}, whether to consider the mean or the sum for the computation of the TIF
#' @param show_all \code{logical}, default is \code{FALSE}. Whether to show the TIF of the test and the TIF target together with the TIF obtained from all the items in \code{item_par}
#' @param show_dist \code{logical}, default is \code{FALSE}. Whether to show or not the difference and distance (absolute difference) from the TIF target.
#' @param ... other arguments
#'
#' @importFrom  stats reshape
#'
#' @returns A \code{ggplot} showing either the TIFs of the test, that of the item bank, and the TIF target or the distance/difference.
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
#' plot(resI)
#' # show the TIF of the item bank
#' plot(resI, show_all = TRUE)
#' # show the distance form the TIF target
#' plot(resI, show_dist = TRUE)
plot.isa <- function(x, fun = "mean",
                              show_all = FALSE,
                     show_dist = FALSE,
                              ...) {
  theta <- x$tif_target$theta
  K <- x$K
  temp <- tif(x$all_iifs, fun = fun)
  alltif <- data.frame(theta = theta,
                       tif = temp$tif,
                       test = paste("all", nrow(x$item_par), "items"))
  tif_target <- x$tif_target
  tif_target$test <- "target"
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
                       test = paste("test with",
                                    nrow(x$selected_items), "items"))
  diff_data <- data.frame(theta = stftif$theta,
                         target_all = tif_target$tif - alltif$tif,
                         target_stf = tif_target$tif - stftif$tif,
                         type = "difference")
  dist_data <- data.frame(theta = diff_data$theta,
                         target_all = abs(diff_data$target_all),
                         target_stf = abs(diff_data$target_stf),
                         type = "distance")
  diff_data <- reshape(diff_data, idvar = c("theta", "type"),
                       timevar = "computation", times = names(diff_data)[2:3],
                       direction = "long", varying = list(names(diff_data)[2:3]),
                       v.names = "value")
  dist_data <- reshape(dist_data, idvar = c("theta", "type"),
                       timevar = "computation", times = names(dist_data)[2:3],
                       direction = "long", varying = list(names(dist_data)[2:3]),
                       v.names = "value")
  plot_diff_data <- rbind(diff_data, dist_data)
  plot_diff_data$computation <- gsub("_stf", "_test", plot_diff_data$computation)
  plottif <- rbind(alltif, stftif, tif_target)
  basic_plot <-   ggplot(plottif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
    geom_line() + theme_light()
  if (show_all == TRUE) {
    basic_plot <- basic_plot
    if (show_dist == TRUE) {
      basic_plot <- ggplot(plot_diff_data,
                          aes(x = .data$theta, y = .data$value,
                              col = .data$computation)) + geom_line(linewidth= 1.2) +
        facet_wrap(~type) + theme_light() + geom_hline(yintercept = 0, linetype = 2,
                                                       col = "black")
    }
  } else {
    basic_plot <- ggplot(plottif[-grep("all", plottif$test), ],
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
      geom_line() + theme_light()
    if (show_dist == TRUE) {
      basic_plot <- ggplot(plot_diff_data[-grep("all", plot_diff_data$computation),],
                           aes(x = .data$theta, y = .data$value,
                               col = .data$computation)) + geom_line(linewidth= 1.2) +
        facet_wrap(~type) + theme_light() + geom_hline(yintercept = 0, linetype = 2,
                                                       col = "black")
    }
  }
  basic_plot <- basic_plot + theme(legend.title = element_blank())
  print(basic_plot)
}
