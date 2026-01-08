#' Define theta targets
#'
#' Define theta targets according to two procedures.
#'
#' @param theta numeric vector defining the latent trait theta
#' @param num_targets integer, define the number of theta targets to obtain
#' @param method character, either equal (default) or clusters
#'
#' @returns A vector of length num_targets with the generated targets
#' @export
#' @importFrom stats kmeans
#'
#' @examples
#' set.seed(123)
#' theta <- rnorm(1000)
#' targets <- define_targets(theta, num_targets = 5, method = "clusters")
define_targets <- function(theta, num_targets = NULL,
                           method = c("equal", "clusters")){
  if (length(method) > 1) {
    method <- "equal"
  }
  if (method == "equal") {
    x <- cut(theta, breaks = num_targets, include.lowest = TRUE)
    pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
    start <- as.numeric(gsub(pattern,"\\2", x))
    end <- as.numeric(gsub(pattern,"\\3", x))
    borders <- data.frame(start, end)
    borders$central <- rowMeans(borders)
    borders <- unique(borders)
    targets <- borders$central
    class(targets) <- "equal"
  } else if (method == "clusters") {
    targets <- kmeans(theta, centers = num_targets)$centers[,1]
    class(targets) <- "clusters"
  }
  return(targets)
}
