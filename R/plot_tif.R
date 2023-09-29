#' plot_tif
#'
#' Plot the test information function of the short test form (default), of the full length test or of both versions
#'
#' @param results A list of length 4 obtained with the functions for shortening tests
#' @param tif character to indicate the TIF to plot, either stf (default), full test or both
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' # Simulate person and item parameters
#' true_theta <- rnorm(100)
#' b <- runif(100, -3, 3)
#' a <- runif(100, 0.6, 2)
#' parameters <- data.frame(b, a)
#' # simulate data
#' data <- sirt::sim.raschtype(true_theta, b = b, fixed.a = a)
#' stf <- uip(data, true_theta = true_theta, item_par = parameters, num_item = 5)
#' # plot the test information function of the full-length test
#' plot(stf, tif = "full")
#' # plot the test information of the full-length test and of the short test form
#' plot(stf, tif = "both")
#' }
plot_tif <- function(results, tif = c("stf", "full", "both")) {
  stf <- data.frame(theta = results$info_stf$theta,
                     info =  results$info_stf$test_info_curve,
                     tif = "short test form")
  full <-  data.frame(theta = results$info_full$theta,
                     info =  results$info_full$test_info_curve,
                     tif = "full-length test")
  both <- rbind(stf, full)
  if (tif == "stf") {
    data <- stf
  } else if (tif == "full") {
    data <- full
  } else if (tif == "both") {
    data <- both
  }

  graph <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data$theta,
                                        y = .data$info, group = 1)) +
    ggplot2::geom_line() + facet_wrap(~.data$tif)
  return(graph)
}
