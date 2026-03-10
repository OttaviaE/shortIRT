#' Define \eqn{\theta} targets
#'
#' Define \eqn{\theta} targets according to two procedures, either by considering the midpoints of equal intervals defined on the latent trait (\code{equal}) or the centroids obtained by clustering the latent trait (\code{clusters}). Further details on targets definition can be found in Epifania et al. (2022).
#'
#' @param theta \code{numeric} vector, define the latent trait \eqn{\theta}
#' @param num_targets \code{integer} value, define the number of \eqn{\theta} targets. The number of \eqn{\theta} targets defines the number of items included in the STF.
#' @param method \code{character}, either \code{equal} (default) or \code{clusters}
#'
#'
#' @references Epifania, O. M., Anselmi, P., & Robusto, E. (2022). Item response
#' theory approaches for test shortening. In M. Wiberg, D. Molenaar,
#' J. Gonzalez, J. S. Kim, & H. Hwang (Eds.), Quantitative Psychology
#' (Vol. 422, pp. 75–83). Springer Proceedings in Mathematics and
#' Statistics. Cham: Springer.
#' https://doi.org/10.1007/978-3-031-27781-8_7

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
