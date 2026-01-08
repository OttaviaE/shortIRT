#' Theta target procedure
#'
#' Procedure based on the theta targets procedure for the generation of a short test form
#'
#' @param targets numeric vector with the discrete values of theta for which the information needs to be maximized
#' @param item_par dataframe, with nrows equals to the length of the latent trait and four columns, each denoting the IRT item parameters
#'
#'
#' @returns
#' A list of length 4 with:
#'
#' - stf: dataframe with the items selected for inclusion in the STF (column isel), their maximum information function (maxiif), for  a specific theta-target (column theta-target)
#' - item_par: the original dataframe containing the item parameters
#' - selected_items: dataframe with the parameters of the selected items
#' - intervals: reports whether the theta-targets were obtained by clustering the latent trait (clusters) or by dividing it into equal intervals (equal). The label "unknown" identifies any other case.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' theta <- rnorm(500)
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#' targets <- define_targets(theta, num_targets = 4)
#' resT <- theta_target(targets, item_par)
#' str(resT)
theta_target <- function(targets, item_par) {

  itarget = t(item_info(ipar = item_par,
                        theta = targets))
  if (inherits(targets, "equal") == FALSE & inherits(targets, "clusters") == FALSE) {
    class_targets  <- "unknown"
  } else {
    class_targets <- class(targets)
  }
  colnames(itarget) = paste("target", targets , sep ="")
  temp = itarget

  isel = numeric(length(targets))
  tsel = numeric(length(targets))
  maxiif = numeric(length(targets))

  for (i in 1:length(targets)) {
    isel[i] = which(temp == max(temp, na.rm = TRUE), arr.ind = TRUE)[1,][1]
    tsel[i] = which(temp == max(temp, na.rm = TRUE), arr.ind = TRUE)[1,][2]
    maxiif[i] =  max(temp, na.rm = TRUE)
    temp[isel[i], ] = NA
    temp[, tsel[i]] = NA
  }
  res <- t(rbind(isel, maxiif))
  res <- data.frame(res)
  res$isel <-  as.character(res$isel)
  res$theta_target <- targets[tsel]
  sel_items <- item_par[res$isel, ]
  results <- list(stf = res,
                  item_par  = item_par,
                  selected_items = sel_items,
                  intervals = class_targets)
  class(results) <- "theta_target"
  return(results)
}
