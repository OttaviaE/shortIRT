#' cut_borders
#'
#' Extract the limits of the intervals obtained from subsetting a vector
#'
#' @param x numeric/integer vector
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(-3, 3, length = 5)
#' groups <- cut(x, 5, include.lowest = TRUE)
#' boundaries <- cut_borders(groups)}
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"

  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))

  data.frame(start, end)
}
