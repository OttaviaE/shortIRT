#' Method for plotting the IIFs
#'
#' Plots the IIFs
#'
#' @param x dataframe of class iifs obtained with the function item_info()
#' @param single_panels logical, default is TRUE. Whether to show the IIFs on different panels
#' @param items default is null (shows all items). Allows for selecting specific items
#' @param ... other arguments
#'
#' @returns A ggplot
#' @export
#' @importFrom utils stack
#'
#' @examples
#' set.seed(123)
#' parameters <- data.frame(b = c(-3,-2,0, 2, 3),
#' a = runif(5, 1.2, 1.9),
#' c = rep(0,5),
#' e= rep(1, 5))
#' infos <- item_info(parameters)
#' plot(infos)
#' # plot only items 1 and 3 on a single panel
#' plot(infos, items = c(1,3), single_panels = FALSE)
plot.iifs <- function(x, single_panels = TRUE,
                      items = NULL, ...) {
  iifs <- stack(x)
  iifs$theta = as.numeric(rownames(x))
  if (is.null(items) == FALSE) {
    iifs <- iifs[iifs$ind %in% items, ]
  }
  basic_plot <- ggplot(iifs,
         aes(x = .data$theta, y = .data$values,
             group = .data$ind, color = .data$ind)) +
    geom_line()
  if (single_panels == TRUE) {
    basic_plot <- basic_plot + facet_wrap(~.data$ind)
  } else {
    basic_plot <- basic_plot
  }
print(basic_plot)
}
