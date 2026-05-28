#' Method for plotting the item information functions
#'
#' Plot the information functions of polytomous or dichotomous items
#'
#' @param x \code{data.frame} of class \code{iifs} obtained with the function \code{item_info()}
#' @param single_panels \code{logical}, default is \code{TRUE}. Whether to show the \eqn{I_i(\theta)} of each item on a different panel
#' @param items default is \code{NULL} (shows all items). Allows for selecting specific items for the plot.
#' @param ... other arguments
#'
#' @details
#' If more there are more than 10 items, the legend associated to the color of the lines is not displayed.
#'
#'
#' @returns A ggplot
#' @export
#' @importFrom utils stack
#'
#' @examples
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Simulate parameters for five dichotomous items
#' # according to a 2-PL specification
#' # b = difficulty parameters
#' # a = discrimination parameters
#' # c = lower asymptote
#' # e = upper asymptote
#' parameters <- data.frame(
#'   b = c(-3, -2, 0, 2, 3),
#'   a = runif(5, 1.2, 1.9),
#'   c = rep(0, 5),
#'   e = rep(1, 5)
#' )
#'
#' # Compute item information functions (IIFs)
#' infos <- item_info(parameters)
#'
#' # Plot information functions for all items
#' plot(infos)
#'
#' # Plot information functions only for items 1 and 3
#' # on a single panel
#' plot(
#'   infos,
#'   items = c(1, 3),
#'   single_panels = FALSE
#' )
plot.iifs <- function(x, single_panels = TRUE,
                      items = NULL, ...) {
  if (inherits(x, "iifs") == FALSE) {
    stop("I need an object of class iifs")
  }
    iifs <- stack(x)
    iifs$theta = as.numeric(rownames(x))
    if (is.null(items) == FALSE) {
      iifs <- iifs[iifs$ind %in% items, ]
    }
    if (ncol(x) > 10) {
      pos <- "none"
    } else {
      pos <- "bottom"
    }
    basic_plot <- ggplot(iifs,
                         aes(x = .data$theta, y = .data$values,
                             group = .data$ind, color = .data$ind)) +
      geom_line() + theme_light() + theme(legend.position = pos)
    if (single_panels == TRUE) {
      basic_plot <- basic_plot + facet_wrap(~.data$ind)
    } else {
      basic_plot <- basic_plot
    }

  basic_plot
}
