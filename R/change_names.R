#' change_names
#'
#' Changes the colnames and stores the orginal colnames of a data frame
#'
#' @param data A data frame
#'
#' @return A list of length two:
#'            1. a data frame with the original colnames and the corresponding new names
#'            2. a data frame with the changed colnames
#' @export
#'
#' @examples
#' \dontrun{
#' # original data frame with 5 columns
#' data <- data.frame(matrix(1:20, nrow = 4, ncol = 5))
#' change_names(data)
#' }
change_names <- function(data) {
  item_names = data.frame(old_names = colnames(data),
                          new_names = gsub('[^0-9.-]', "item", colnames(data)))
  colnames(data) = gsub("I00|I0|I",
                        "item", gsub('[^0-9.-]', "item", colnames(data)))
  new <- list(item_names = item_names, data = data)
  return(new)
}
