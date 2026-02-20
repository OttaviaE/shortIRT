#' Method for plotting the TIF of the STF
#'
#' The STF is obtained with the benchmark procedure implemented with function \code{bench()}
#'
#' @param x Object of class \code{bench}
#' @param fun \code{character}, whether to consider the mean or the sum for the computation of the TIF
#' @param show_both \code{logical}, default is \code{TRUE}. Whether to show or not the TIF obtained from the full-length test
#' @param ... other arguments
#'
#' @returns A \code{ggplot} showing the TIFs of both the STF and the full-length test, unless otherwise specified
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
#' # same but with polytomous items
#' item_pars <- data.frame(matrix(c(
#'  1.2, 1.0, 0.8,  -1.0, 0.0, 1.2,
#'  0.9, 1.1, 1.3,  -0.5, 0.7, 1.8,
#'  0.5, 1.5, 1, -1.5, -1.0, 0,
#'  1, 1, 1, -1.5, -0, 0.5
#'  ), nrow = 4, byrow = TRUE))
#' colnames(item_pars) = paste(rep(c("a", "b"), each = 3), 1:3, sep = "")
#' resB_poly <- bench(item_pars, theta = theta, num_item = 2, K = 3)
#' plot(resB_poly)
plot.bench <- function(x, fun = "sum",
                             show_both = TRUE,
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
      stfiif <- iif_poly(x$selected_items, theta = theta, K = K)
    }
  temp <- tif(stfiif, fun = fun)
  stftif <- data.frame(theta = temp$theta,
                       tif = temp$tif,
                       test = paste("stf with",
                                    nrow(x$selected_items), "items"))
  plottif <- rbind(alltif, stftif)
  if (show_both == TRUE) {
    basic_plot <-  ggplot(plottif,
                          aes(x = .data$theta, y = .data$tif,
                              group = .data$test, col = .data$test)) +
      geom_line() + theme_light()
  } else  {
    basic_plot <- ggplot(stftif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
      geom_line() + theme_light()
  }
  print(basic_plot)
}
