#' Change column names
#'
#' Changes the columns names of a data frame and stores the original colum names
#'
#' @param data A data frame
#'
#' @returns
#' A list of length two:
#'
#' 1. a data frame with the original column names and the corresponding new names
#'
#' 2. a data frame with the changed column names
#' @export
#'
#' @examples
#' # original data frame with 5 columns
#' data <- data.frame(matrix(1:20, nrow = 4, ncol = 5))
#' change_names(data)
change_names <- function(data) {
  item_names = data.frame(old_names = colnames(data),
                          new_names = gsub('[^0-9.-]', "item", colnames(data)))
  colnames(data) = gsub('[^0-9.-]', "item", colnames(data))
  new <- list(item_names = item_names, data = data)
  return(new)
}
