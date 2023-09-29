#' bp
#'
#' Create a Short Test Form (STF) using the typical IRT procedure for shortening test (Benchmark Procedure, BP)
#'
#' @param data A subject x item matrix
#' @param item_par Two column matrix with the item parameters. The first column must contain the difficulty parameters, the second column must contain the discrimination parameters.
#' @param seed A random seed (default is 999)
#' @param starting_theta A vector with length equal to the number of rows in data with the true theta values of the subjects. If empty, the theta values will be estimated from the data.
#' @param num_item the number of item to included in the short test form
#'
#' @return A list of length 5:
#'          - item_stf: data frame with nrows equal to the number of items included in the STF, contains the items, the theta targets and the IIF in respect to the theta target
#'          - summary: data frame with two rows, contains the list of items included in the STF and the total information, along with the information of the full length test
#'          - info_stf: contains the information for each level of the latent trait for the STF
#'          - info_full: contains the information for each level of the latent trait for the full-length test
#'          - theta: data frame with the starting theta and the theta estimated with the STF
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Simulate person and item parameters
#' starting_theta = rnorm(100)
#' b <- runif(100, -3, 3)
#' a <- runif(100, 0.6, 2)
#' parameters <- data.frame(b, a)
#' # simulate data
#' data <- sirt::sim.raschtype(starting_theta, b = b, fixed.a = a)
#' stf <- bp(data, starting_theta = starting_theta, item_par = parameters, num_item = 5)
#' # check the obtained short test form
#' stf$item_stf
#' # check the comparison between the short test form and the full-length test
#' stf$summary
#' }
bp <- function(data,
                item_par = NULL,
                seed = 999,
                starting_theta = NULL,
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

  if (!is.null(starting_theta)) {
    if (length(starting_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    starting_theta <- start_model$EAP
  }
  item_names <- change_names(data)$item_names
  data <- change_names(data)$data

  info_start <- mean(TAM::IRT.informationCurves(start_model,
                                           theta = starting_theta)$test_info_curve)
  info_full <- TAM::IRT.informationCurves(start_model,
                                            theta = starting_theta)


  lab_item <- 1:ncol(data)
  num_item <- num_item


  data_info_bp <- data.frame(items = 1:(ncol(data)),
                             item_info = numeric((ncol(data))))

  for (i in 1:nrow(data_info_bp)) {
    data_info_bp[i, "item_info"] <- mean(TAM::IRT.informationCurves(start_model,
                                                          theta = starting_theta,
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
  item_names <- item_names[selected_items$items, ]
  selected_bp <- selected_items
  colnames(selected_bp)[1] <- "item"
  selected_bp$item <- item_names$old_names


    out_bp <- data[, selected_items$items]
    model_out_bp <- TAM::tam.mml(out_bp,
                                 xsi.fixed = cbind(1:ncol(out_bp),
                                                   b_true[as.integer(gsub("item", '',
                                                                             colnames(out_bp))), 2]),
                                 B= array(c(rep(0, ncol(out_bp)),
                                            a_true[,2,][as.integer(gsub("item", "",
                                                                            colnames(out_bp)))]),
                                          c(ncol(out_bp),2,1),
                                          dimnames = list(colnames(out_bp),
                                                          c("Cat0", "Cat1"),
                                                          "Dim01")),
                            verbose = FALSE)

    info_out_bp <- TAM::IRT.informationCurves(model_out_bp,
                                              theta = starting_theta)


  # Summary
  info_summary_bp <- NULL
  temp <- NULL

  info_summary_bp <- data.frame(info_test = mean(info_out_bp$test_info_curve),
                       bp_name = paste0("STF-", num_item),
                       item = paste(item_names[item_names$new_names %in% colnames(out_bp),
                                               "old_names"], collapse = ", "))

  info_summary_bp <-  rbind(info_summary_bp,
                            data.frame(info_test = (info_start),
                                       bp_name = "all",
                                       item = "all"))
  info_summary_bp$selection <- "BP"

  theta <- data.frame(starting_theta = starting_theta,
                      stf_theta = model_out_bp$person$EAP)
  bp_results = list(item_stf = selected_bp,
                     summary = info_summary_bp,
                     info_stf = info_out_bp,
                    info_full =info_full,
                     theta = theta)

  return(bp_results)
}
