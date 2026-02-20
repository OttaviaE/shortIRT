#' Define \eqn{\theta} targets
#'
#' Define \eqn{\theta} targets according to two procedures, either by considering the midpoints of equal intervals defined on the latent trait (\code{equal}) or the centroids obtained by clustering the latent trait (\code{clusters})
#'
#' @param theta \code{numeric} vector defining the latent trait \eqn{\theta}
#' @param num_targets \code{integer} value, define the number of \eqn{\theta} targets. The number of \eqn{\theta} targets defines the number of items included in the STF.
#' @param method \code{character}, either \code{equal} (default) or \code{clusters}
#'
#' @returns A vector of length \code{num_targets} with the generated \eqn{\theta} targets. The class can be either \code{equal} or \code{clusters}, dependening on the method used for the definition of the \eqn{\theta} targets
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
  if (is.null(num_targets)) {
    stop("Must define a number of theta targets")
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
