#' plot_difference
#'
#' Plot the difference or the absolute difference between the starting theta and the theta estimated with the stf as a function of different levels of the latent trait
#'
#' @param difference data frame obtained with the function diff_theta
#' @param type type of difference, either as is (diff) or absolute (absolute_diff)
#' @param levels number of levels of the starting theta (default is 4)
#'
#' @importFrom dplyr %>%
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate person and item parameters
#' starting_theta = rnorm(100)
#' b <- runif(100, -3, 3)
#' a <- runif(100, 0.6, 2)
#' parameters <- data.frame(b, a)
#' # simulate data
#' data <- sirt::sim.raschtype(starting_theta, b = b, fixed.a = a)
#' stf <- uip(data, starting_theta = starting_theta, item_par = parameters, num_item = 5)
#' # compute the difference between starting theta and that estimated with the stf
#' my_diff <- diff_theta(stf)
#' # plot the difference with default number of levels
#' plot_difference(my_diff, type = "diff")
#' # plot the absolute difference with 10 levels
#' plot_difference(my_diff, type = "abs_diff", levels = 10)
#' }
plot_difference <- function(difference,
                            type = c("diff", "absolute_diff"),
                            levels = 4) {
  difference$levels <- ggplot2::cut_number(difference[,
                                                         grepl("starting|true",
                                                               colnames(difference))],
                                              levels)
  mean_diff <- difference %>%
    dplyr::group_by(levels) %>%
    dplyr::summarise(mean = mean(.data$difference),
                     mean_abs = mean(.data$abs_difference))
  if (type == "diff") {
    graph <- ggplot2::ggplot(mean_diff,
                    aes( x = .data$levels, y = .data$mean, group = 1)) + geom_line()
    min_y <- -abs(min(difference[,
                                 grepl("starting|true",
                                       colnames(difference))]))-1
    ylab <- "Difference"
  } else if (type == "absolute_diff") {
    graph <- ggplot2::ggplot(mean_diff,
                             ggplot2::aes( x = .data$levels,
                                           y = .data$mean_abs, group = 1)) + ggplot2::geom_line()
    min_y <- 0
    ylab <- "Absolute difference"
  }
  graph <- graph + ggplot2::ylab(type) + ggplot2::ylim(min_y,
                                     abs(min(difference[,
                                                        grepl("starting|true",
                                                              colnames(difference))]))+1) +
    ggplot2::ylab(ylab)

  return(graph)
}
