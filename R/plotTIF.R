#' Plot Test Information Function (TIF)
#'
#'
#' @param object Object of either class \code{shortForm} or \code{compareSHORT}
#' @param graph plot the TIF for the test including all items (\code{all}), the short form (\code{short}) or together (\code{together})
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return a ggplot
#' @export
#'
#' @examples
plotTIF = function(object,
                   graph = c("all", "short", "together")) {

  graph = match.arg(graph)

  if (is.na(class(object)[2])) {
    stop("Object must be of class shortForm or compareSHORT")
  }

  if (class(object)[2] == "compareSHORT") {
    info_compare = NULL
    for (i in 1:length(object)) {
      object[[i]]$info_data$strategy = names(object)[[i]]
      info_compare = rbind(info_compare, object[[i]]$info_data)
    }

    ggplot2::ggplot(info_compare,
                    ggplot2::aes(x = .data$theta, y = .data$info,
               group = .data$type, color = .data$type)) +
      ggplot2::geom_line() + ggplot2::facet_wrap(~strategy)
  } else {
    info_data = object$info_data


    if(graph == "all") {
      ggplot2::ggplot(info_data[info_data$type %in% "all", ],
                      ggplot2::aes(x = .data$theta, y = .data$info,
                                   group = .data$type, color = .data$type)) +
        ggplot2::geom_line() + ggplot2::ggtitle(paste(class(object)[3],
                                                      "TIF: All item")) +
        ggplot2::theme(legend.position = "top")
    } else if (graph == "short") {
      ggplot2::ggplot(info_data[info_data$type %in% "short", ],
                      ggplot2::aes(x = .data$theta, y = .data$info,
                                   group = .data$type, color = .data$type)) +
        ggplot2::geom_line() + ggplot2::ggtitle(paste(class(object)[3],
                                                      "TIF: Short form")) +
        ggplot2::theme(legend.position = "top")
    } else if (graph == "together") {
      ggplot2::ggplot(info_data,
                      ggplot2::aes(x = .data$theta, y = .data$info,
                                   group = .data$type, color = .data$type)) +
        ggplot2::geom_line() + ggplot2::ggtitle(paste(class(object)[3],
                                                      "TIF")) +
        ggplot2::theme(legend.position = "top")
    }
  }


}
