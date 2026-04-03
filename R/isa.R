#' Item Selection Algorithm
#'
#' Develop a test or a short form given the parameters of dichotomous or polytomous in an item bank/full-length test according to the Item Selection procedure (ISA, Epifania & Finos, 2025). See \code{Details}.
#'
#' @param item_pars \code{data.frame}, dataframe with number of rows equal to the number of items.
#'    For dichotomous items, the dataframe must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the dataframe has \eqn{2K} columns, where \eqn{K} is the number of thresholds of the items (number of response categorie \eqn{- 1}). The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters
#'   \eqn{b_1, \dots, b_K} (must be named "b").
#' @param tif_target \code{data.frame} with two columns: \code{theta} the latent trait \eqn{\theta} and \code{tif} defining the values of the tif target. The TIF target should be computed as the mean TIF to allow for the comparability with the TIF obtained from the test.
#' @param nmin \code{numeric}, minimum number of items to be included in the test (i.e., the termination criterion is not tested until the minimum number of items is reached). Default is the 10\% of the total number of items.
#' @param K \code{integer}, number of thresholds for polytomous items (number of response categories minus 1). Default is \code{NULL} (assumes dichotomous items).
#' @details
#' Let \eqn{t = 0, \dots, T} denote the iteration index of the procedure, \eqn{\text{TIF}^*} denote the test information target, and \eqn{\text{TIF}^t} denote the test information function obtained from \eqn{Q^t \subset B} (where \eqn{B} is the item bank and \eqn{Q^t} is the subset of items selected up to iteration \eqn{t}).
#' At \eqn{t = 0}: \eqn{\text{TIF}^0(\theta) = 0}, \eqn{\forall \theta}, \eqn{Q^0 = \emptyset}.
#' For \eqn{t \geq 0},
#'
#' \enumerate{
#'   \item Consider the available items at iteration \eqn{t}
#'
#'   \deqn{A^t = B \setminus Q^t}
#'
#'   \item Compute the provisional TIF (\eqn{\text{pTIF}_i}) for each of the available items
#'
#'   \deqn{\forall i \in A^t, \text{pTIF}_{i} := \frac{\text{TIF}^t + I_{i}(\theta)}{|Q^t|+1}}
#'
#'   \item Select a provisional item \eqn{i^*} allowing for minimizing the distance from the TIF target
#'
#'   \deqn{i^* := \arg \min_{i \in A^t} \text{abs}(\text{TIF}^* - \text{pTIF}_i)}
#'
#'   \item Test the termination criterion: If
#'
#'   \eqn{\text{abs}(\text{TIF}^* - \text{pTIF}_{i^*}) \ngeq \text{abs}(\text{TIF}^* - \text{TIF}^{t}), Q^{t+1} = Q^{t} \cup \{i^+\}}, \eqn{\text{TIF}^{t+1} = \text{pTIF}_{i^+}}, Go back to 1
#'
#'   \item Stop, \eqn{Q_{\text{ISA}} = Q^t}
#'
#' }
#'
#' Further details on the algorithm can be found in Epifania & Finos (2025), where the algorithm is denoted as Frank.
#'
#'
#' @returns
#' An object of class \code{isa} of length 6 containing:
#'
#' \itemize{
#'   \item \strong{test}: a dataframe containing the items selected for inclusion
#'   in the test (column \code{isel}) and the minimum number of items set in \code{nmin}.
#'
#'   \item \strong{item_pars}: the original dataframe containing the item parameters.
#'
#'   \item \strong{selected_items}: a dataframe containing the parameters of the selected items.
#'
#'   \item \strong{all_iifs}: a dataframe containing the IIFs of all the original items.
#'
#'   \item \strong{tif_target}: the dataframe with the TIF target used for the item selection.
#'
#'   \item \strong{K}: Number of thresholds for the response categories of the items. If the items are dichotomous \code{K} is \code{NULL}.
#' }
#' @export
#'
#' @references Epifania, O. M., & Finos, L. (2025). Nothing lasts forever – only item
#' administration: An item response theory algorithm to shorten tests.
#' In E. Di Bella, V. Gioia, C. Lagazio, & S. Zaccarin (Eds.),
#' Statistics for Innovation III (pp. 188–193).
#' Italian Statistical Society Series on Advances in Statistics.
#' Springer, Cham. https://doi.org/10.1007/978-3-031-95995-0_32
#'
#' @examples
#' set.seed(123)
#'
#' n <- 50
#' theta <- rnorm(500)
#'
#' item_pars <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#'
#' tif_target <- tif(
#'   item_info(item_pars[-c(3, 5, 10, 14), ]),
#'   fun = "mean"
#' )
#'
#' resIsa <- isa(item_pars, tif_target, nmin = 4)
#' str(resIsa)
#'
#' # Polytomous items with user-defined theta targets
#'
#' item_pars <- data.frame(
#'   matrix(c(
#'     1.2, 1.0, 0.8,  -1.0,  0.0,  1.2,
#'     0.9, 1.1, 1.3,  -0.5,  0.7,  1.8,
#'     0.5, 1.5, 1.0,  -1.5, -1.0,  0.0,
#'     1.0, 1.0, 1.0,  -1.5,  0.0,  0.5
#'   ), nrow = 4, byrow = TRUE)
#' )
#'
#' colnames(item_pars) <- paste(
#'   rep(c("a", "b"), each = 3),
#'   1:3,
#'   sep = ""
#' )
#'
#' resT_poly <- theta_target(c(-1, 0), item_pars, K = 3)
#' str(resT_poly)
isa <- function(item_pars, tif_target, nmin = round(nrow(item_pars)*0.10), K = NULL) {
  if ( attributes(tif_target)$source  != "mean") {
    warning("we strongly advise for the use of the mean TIF for the definition of the TIF target")
  }
  if (is.null(K)) {
    if (length(unique(gsub("[0-9]", "", colnames(item_pars)))) == 2) {
      stop("You forgot to specifiy the number of thresholds for your items!")
    }
  }
  theta <- tif_target$theta
  original_parameters <- item_pars
  token <- TRUE
  j <- 0
  iif_stf <- matrix(,length(tif_target$theta), 0)
  distance_target_tif = Inf
  iifs <- item_info(item_pars, theta, K = K)
  while (token == TRUE) {
    j = j +1
    item_indexes = which(!is.na(item_pars[,1]))
    # adesso il ciclo for itera negli item indexes e calcola la PIF
    difference = rep(NA, nrow(item_pars))
    for (i in item_indexes) {
      all_iifs = iifs[,i]
      pif = data.frame(cbind(iif_stf, all_iifs))
      pif = data.frame(rowMeans(pif))
      # difference[i] = mean(abs(((target$mean_tif - pif[,1])/target$mean_tif)))
      difference[i] = mean(abs(tif_target$tif - pif)[,1])
    }
    # qui trovo l'item che minimizza e lo metto in d
    d_index = which(difference == min(difference, na.rm = T))
    iif_stf = data.frame(iif_stf,  iifs[,d_index])
    colnames(iif_stf)[ncol(iif_stf)] = paste("item", d_index, sep = "_")
    # guardo le differenze
    if (j <= nmin) {
      token = TRUE
      item_pars[d_index, ] = NA
      distance_target_tif = mean(abs(tif_target$tif - rowMeans(iif_stf)))
      # distance_target_tif = mean(abs(((tif_target$mean_tif - rowMeans(iif_stf))/tif_target$mean_tif)))

    } else if (difference[d_index] >= distance_target_tif) {
      token = FALSE
      temp_item = colnames(iif_stf)[-ncol(iif_stf)]
      temp_item = as.numeric(gsub("item_", "", temp_item))
      temp_item = temp_item[order(temp_item)]
      sel_items = paste("item", temp_item, sep = "_")
      sel_items = paste(sel_items, collapse = " ")
    } else {
      item_pars[d_index, ] = NA
      distance_target_tif = mean(abs(tif_target$tif - rowMeans(iif_stf)))
      # distance_target_tif = mean(abs(((tif_target$mean_tif - rowMeans(iif_stf))/tif_target$mean_tif)))
    }
  }
  iif_stf = data.frame(iif_stf[,-ncol(iif_stf)])
  if (ncol(iif_stf) == 1) {
    colnames(iif_stf) = sel_items
  }
  isel <- as.numeric(gsub("item_", "", colnames(iif_stf)))
  stf_info = data.frame(isel = rownames(item_pars)[isel], nmin = nmin)
  rownames(iifs) <- theta
  results <- list(test = stf_info,
                  item_pars  = original_parameters,
                  selected_items = original_parameters[isel, ],
                  all_iifs = iifs,
                  tif_target = tif_target,
                  K = K)
  class(results) <- "isa"
  return(results)
}
