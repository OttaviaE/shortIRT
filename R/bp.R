#' bp
#'
#' Create a Short Test Form (STF) using the typical IRT procedure for shortening test
#'
#' @param data A subject x item matrix
#' @param item_par Two column matrix with the item parameters. The first column must contain the difficulty parameters, the second column must contain the discrimination parameters.
#' @param seed A random seed (default is 999)
#' @param true_theta A vector with length equal to the number of rows in data with the true theta values of the subjects. If empty, the theta values will be estimated from the data.
#' @param num_item the number of item to included in the short test form
#'
#' @return A list of length 3:
#'          - item_stf: data frame with as many rows as the the number of items to be included in the STF.
#'          - summary: contains the list of items included in the STF and the total information, along with the information of the full length test
#'          - info_stf: contains the information for each level of the latent trait. used for plotting
#' @export
#'
#' @import TAM
#' @import sirt
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Simulate person and item parameters
#' true_theta = rnorm(100)
#' b <- runif(100, -3, 3)
#' a <- runif(100, 0.6, 2)
#' parameters <- data.frame(b, a)
#' # simulate data
#' data <- sirt::sim.raschtype(true_theta, b = b, fixed.a = a)
#' stf <- bp(data, true_theta = true_theta, item_par = parameters, num_item = 5)
#' # check the obtained short test form
#' stf$item_stf
#' # check the comparison between the short test form and the full-length test
#' stf$summary
#' }
bp <- function(data,
                item_par = NULL,
                seed = 999,
                true_theta = NULL,
                num_item = NULL) {
  if (is.null(num_item)) {
    stop("You must specify the number of items for the STFs!")
  }
  if(is.null(item_par)) {
    start_model <- TAM::tam.mml.2pl(data, verbose = FALSE, irtmodel = "2PL")
    b_true <- matrix(cbind(1:length(start_model$item$xsi.item),
                           start_model$item$xsi.item),
                     ncol = 2)
    a_true <- array(c(rep(0, length(start_model$item$B.Cat1.Dim1)), start_model$item$B.Cat1.Dim1),
                    c(length(start_model$item$B.Cat1.Dim1),2,1),
                    dimnames = list(paste0("I", 1:length(start_model$item$B.Cat1.Dim1)),
                                    c("Cat0", "Cat1"),
                                    "Dim01"))
  } else {
    b_true <- matrix(cbind(1:nrow(item_par),
                           item_par[,1]),
                     ncol = 2)
    a_true <- array(c(rep(0, nrow(item_par)), item_par[,2]),
                    c(nrow(item_par),2,1),
                    dimnames = list(paste0("I", 1:nrow(item_par)),
                                    c("Cat0", "Cat1"),
                                    "Dim01"))
    start_model = TAM::tam.mml(resp=data, xsi.fixed = b_true, B = a_true, verbose = FALSE)
  }

  if (!is.null(true_theta)) {
    if (length(true_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    true_theta <- start_model$EAP
  }

  info_start <- mean(TAM::IRT.informationCurves(start_model,
                                           theta = true_theta)$test_info_curve)
  info_start_theta <- TAM::IRT.informationCurves(start_model,
                                            theta = true_theta)


  lab_item <- 1:ncol(data)
  num_item <- num_item


  data_info_bp <- data.frame(items = 1:(ncol(data)),
                             item_info = numeric((ncol(data))))

  for (i in 1:nrow(data_info_bp)) {
    data_info_bp[i, "item_info"] <- mean(TAM::IRT.informationCurves(start_model,
                                                          theta = true_theta,
                                                          iIndex = lab_item[i])$info_curves_item)

  }

  data_info_bp$stf_length <- paste0("STF-", num_item)

  # given the number(s) of items in num_item, the items with the highest IIFs
  # are selected.

  data_info_bp <- data_info_bp[order(data_info_bp$item_info, decreasing = TRUE), ]

  # given the number(s) of items in num_item, filter out the selected ones from the
  # full-length test, estimate the model on the resulting short form(s), and
  # compute the IIF and TIF
  # (bp = benchmark procedure)

  selected_items <- data_info_bp[1:num_item, ]
  selected_bp = selected_items
    out_bp <- data[, selected_items$items]
    model_out_bp <- TAM::tam.mml(out_bp,
                                 xsi.fixed = cbind(1:ncol(out_bp),
                                                   b_true[as.integer(gsub("I00|I0|I", '',
                                                                             colnames(out_bp))), 2]),
                                 B= array(c(rep(0, ncol(out_bp)),
                                            a_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                            colnames(out_bp)))]),
                                          c(ncol(out_bp),2,1),
                                          dimnames = list(colnames(out_bp),
                                                          c("Cat0", "Cat1"),
                                                          "Dim01")),
                            verbose = FALSE)

    info_out_bp <- TAM::IRT.informationCurves(model_out_bp,
                                              theta = true_theta)


  # Summary
  info_summary_bp <- NULL
  temp <- NULL

  info_summary_bp <- data.frame(info_test = mean(info_out_bp$test_info_curve),
                       bp_name = paste0("STF-", num_item),
                       item = paste(colnames(out_bp), collapse = ","))

  info_summary_bp <-  rbind(info_summary_bp,
                            data.frame(info_test = (info_start),
                                       bp_name = "all",
                                       item = "all"))
  info_summary_bp$selection <- "BP"


  bp_results = list(item_stf = selected_bp,
                     summary = info_summary_bp,
                     info_stf = info_out_bp,
                    info_start =info_start_theta)
  return(bp_results)
}
