#' plot_tif
#'
#' Plot the test information function of the short test form (default), of the full length test or of both versions
#'
#' @param results A list of length 4 obtained with the functions for shortening tests
#' @param tif character to indicate the TIF to plot, either stf (default), full test or both
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
plot_tif <- function(results, tif = c("stf", "full", "both")) {
  stf <- data.frame(theta = results$info_stf$theta,
                     info =  results$info_stf$test_info_curve,
                     tif = "stf")
  full <-  data.frame(theta = results$info_full$theta,
                     info =  results$info_full$test_info_curve,
                     tif = "full")
  both <- rbind(stf, full)
  if (tif == "stf") {
    data <- stf
  } else if (tif == "full") {
    data <- full
  } else if (tif == "both") {
    data <- both
  }

  graph <- ggplot2::ggplot(data,
                           ggplot2::aes(x = theta, y = info,
                                        color = tif, group = tif)) + ggplot2::geom_line()
  return(graph)
}
