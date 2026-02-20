#' ISA
#'
#' @param item_pars
#' @param tif_target
#' @param nmin
#' @param K
#'
#' @returns
#' @export
#'
#' @examples
isa <- function(item_pars, tif_target, nmin = round(nrow(item_pars)*0.10), K = NULL) {
  if ( attributes(target)$source  != "mean") {
    warning("we strongly advise for the use of the mean TIF for the definition of the TIF target")
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
  stf_info = data.frame(isel = isel, nmin = nmin)
  rownames(iifs) <- theta
  results <- list(stf = stf_info,
                  item_pars  = original_parameters,
                  selected_items = original_parameters[isel, ],
                  all_iifs = iifs,
                  tif_target = tif_target,
                  K = K)
  class(results) <- "isa"
  return(results)
}
