#' Method for plotting the TIF of the test/short test form
#'
#' The test/short test form is obtained with the benchmark procedure implemented with function \code{bench()}. Details on the procedure can be found in the documentation of the \code{bench()} function.
#'
#' @param x Object of class \code{bench}
#' @param fun \code{character}, whether to consider the mean or the sum for the computation of the TIF
#' @param ... other arguments
#'
#' @returns A \code{ggplot} showing the TIFs of the test.
#' @export
#'
#' @examples
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Define the number of items
#' n <- 50
#'
#' # Generate latent trait values for 500 respondents
#' # using a wider latent distribution (sd = 2)
#' theta <- rnorm(500, sd = 2)
#'
#' # Create item parameter matrix/data frame
#' # b = difficulty parameters
#' # a = discrimination parameters
#' # c = lower asymptote
#' # e = upper asymptote
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#'
#' # Run benchmark/selection procedure
#' # selecting 5 items from the item pool
#' resB <- bench(
#'   item_par,
#'   theta = theta,
#'   num_item = 5
#' )
#'
#' # Plot benchmark results including
#' # item-level and test-level information
#' plot(resB)
#'
#' # Plot only the Test Information Function (TIF)
#' plot(resB, show_both = FALSE)
#'
#' # Define parameters for four polytomous items
#' # with four response categories (K = 3 thresholds)
#' item_pars <- data.frame(
#'   matrix(
#'     c(
#'       1.2, 1.0, 0.8,  -1.0,  0.0, 1.2,
#'       0.9, 1.1, 1.3,  -0.5,  0.7, 1.8,
#'       0.5, 1.5, 1.0,  -1.5, -1.0, 0.0,
#'       1.0, 1.0, 1.0,  -1.5,  0.0, 0.5
#'     ),
#'     nrow = 4,
#'     byrow = TRUE
#'   )
#' )
#'
#' # Assign parameter names
#' colnames(item_pars) <- paste(
#'   rep(c("a", "b"), each = 3),
#'   1:3,
#'   sep = ""
#' )
#'
#' # Run benchmark/selection procedure
#' # for polytomous items selecting 2 items
#' resB_poly <- bench(
#'   item_pars,
#'   theta = theta,
#'   num_item = 2,
#'   K = 3
#' )
#'
#' # Plot benchmark results for the polytomous case
#' plot(resB_poly)
plot.bench <- function(x, fun = "sum",
                              ...) {
  if (inherits(x, "bench") == FALSE) {
    stop("I need an object of either class bench or theta target")
  }
  theta <- as.numeric(rownames(x$all_iifs))
  if (!is.null(x$item_pars)) {
    K <- x$K
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
  } else {
    temp <- tif(x$all_iifs, fun = fun)
  }
  stftif <- data.frame(theta = temp$theta,
                       tif = temp$tif,
                       test = paste("test with",
                                    nrow(x$selected_items), "items"))

    basic_plot <- ggplot(stftif,
                         aes(x = .data$theta, y = .data$tif,
                             group = .data$test, col = .data$test)) +
      geom_line() + theme_light() + theme(legend.title = element_blank())
  basic_plot
}
