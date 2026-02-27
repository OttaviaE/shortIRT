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
    theta_ord <- sort(theta)
    width  <- (max(theta_ord) - min(theta_ord)) / num_targets
    breaks <- min(theta_ord) + 0:num_targets * width

    intervals <- data.frame(
      start   = breaks[-length(breaks)],
      end     = breaks[-1]
    )
    targets <- rowMeans(intervals)
    class(targets) <- "equal"
  } else if (method == "clusters") {
    targets <- kmeans(theta, centers = num_targets)$centers[,1]
    class(targets) <- "clusters"
  }
  return(targets)
}
