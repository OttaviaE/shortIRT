#' eip
#'
#' Create a Short Test Form (STF) using the theta-target procedure based on the equal segmentation of the latent trait
#'
#' @param data a subject x item matrix
#' @param fixed.b a vector of fixed difficulty parameters. If empty, the item parameters estimated from the data will be used.
#' @param fixed.a a vector of fixed discrimination parameters. If empty, the item parameters estimated from the data will be used.
#' @param seed a random seed (default is 999)
#' @param true_theta a vector with length equal to the number of rows in data with the true theta values of the subjects. If empty, the theta values will be estimated from the data.
#' @param num_item the number of item to included in the short test form
#'
#' @import TAM
#' @importFrom sirt sim.raschtype
#'
#' @return A list of length 3
#' @export
#'
#' @examples
eip <- function(data,
                fixed.b = NULL,
                fixed.a = NULL,
                seed = 999,
                true_theta = NULL,
                num_item = NULL,
                theta_targets = NULL) {
  if (is.null(num_item) & is.null(theta_targets)) {
    stop("You must specify a number of items for the STFs or a vector of theta targets!")
  }

  if(is.null(fixed.a) & is.null(fixed.b)) {
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
    b_true <- matrix(cbind(1:length(fixed.b),
                           fixed.b),
                     ncol = 2)
    a_true <- array(c(rep(0, length(fixed.a)), fixed.a),
                    c(length(fixed.a),2,1),
                    dimnames = list(paste0("I", 1:length(fixed.a)),
                                    c("Cat0", "Cat1"),
                                    "Dim01"))

    start_model = TAM::tam.mml(resp=data, xsi.fixed = b_true, B = a_true, verbose = FALSE)
  }

  info_start <- mean(TAM::IRT.informationCurves(start_model,
                                           theta = true_theta)$test_info_curve)

  lab_item <- 1:ncol(data)
  num_item <- num_item

  if (!is.null(true_theta)) {
    if (length(true_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    true_theta <- start_model$EAP
  }

  if (!is.null(theta_targets)) {
    cut_value <- theta_targets
  } else {
    intervals <- NULL
    groups <- NULL
    cut_value <- NULL
    cut_value_temp <- NULL

    for (i in 1:(length(num_item))) {
      intervals <- seq(min(true_theta), max(true_theta),
                       length =num_item[i])
      groups <- cut(intervals, num_item[i], include.lowest = TRUE)
      cut_value_temp <- cut_borders(groups)
      cut_value = rbind(cut_value_temp, cut_value)
    }
    cut_value$mean_theta <- rowMeans(cut_value)
    cut_value <- cut_value$mean_theta
  }


  # Compute IIF for each theta target
  info_test <- NULL
  temp <- list()
  value <- list()
  temp_data <- NULL
  info_data <- NULL


    for(i in 1:length(lab_item)) {
      for(j in 1:length(cut_value)) {

        # temp_data <- data.frame(theta_target = IRT.informationCurves(start_model,
        #                                                              theta = cut_value[j,
        #                                                                                 "mean_theta"],
        #                                                              iIndex = lab_item[i])$theta,
        #                         item_info = mean(colSums(IRT.informationCurves(start_model,
        #                                                                        theta = cut_value[j,
        #                                                                                           "mean_theta"],
        #                                                                        iIndex = lab_item[i])$info_curves_item)),
        #                         item = lab_item[i],
        #                         num_item = paste("STF-", nrow(cut_value), sep = ""))

        temp_data <- data.frame(theta_target = TAM::IRT.informationCurves(start_model,
                                                                     theta = cut_value[j],
                                                                     iIndex = lab_item[i])$theta,
                                item_info = mean(colSums(TAM::IRT.informationCurves(start_model,
                                                                               theta = cut_value[j],
                                                                               iIndex = lab_item[i])$info_curves_item)),
                                item = lab_item[i],
                                num_item = paste("STF-", nrow(cut_value), sep = ""))

        info_data <- rbind(info_data, temp_data)
      }
    }

  # select the item with highest IIF for each theta target
  if (length(unique(cut_value)) > 1) {
    temp_maxinfo <- aggregate(item_info ~ item + theta_target,
                              data = info_data, max)
    temp_maxinfo$eip_name <- unique(info_data$num_item)

    temp <- NULL
    max_temp <- NULL

    for (i in 1:length(unique(temp_maxinfo$theta_target))) {
      temp <- temp_maxinfo[which(temp_maxinfo$item_info == max(temp_maxinfo$item_info)), ]
      temp_maxinfo <- temp_maxinfo[which(temp_maxinfo$item != temp$item &
                                           temp_maxinfo$theta_target != temp$theta_target), ]
      max_temp <-rbind(max_temp, temp)

    }

    } else {
    info_data <- info_data[order(info_data$item_info, decreasing = TRUE),]
    max_temp <- dplyr::distinct(info_data)
    max_temp <- max_temp[1:length(theta_targets), ]
    max_temp$eip_name <- unique(info_data$num_item)
  }



  selected_eip = max_temp[order(max_temp$theta_target), ]

  # given the number(s) of items in num_item, filter out the selected ones from the
  # full-length test, estimate the model on the resulting short form(s), and
  # compute the IIF and TIF



    out_eip <- data[, c(max_temp[max_temp$eip_name %in%unique(max_temp$eip_name),
                                      "item"])]
    model_out_eip <- TAM::tam.mml(out_eip,
                                  xsi.fixed = cbind(1:ncol(out_eip),
                                                    b_true[as.integer(gsub("I00|I0|I", '',
                                                                              colnames(out_eip))), 2]),
                                  B= array(c(rep(0, ncol(out_eip)),
                                             a_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                             colnames(out_eip)))]),
                                           c(ncol(out_eip),2,1),
                                           dimnames = list(colnames(out_eip),
                                                           c("Cat0", "Cat1"),
                                                           "Dim01")),
                             verbose = FALSE)
    info_out_eip <- TAM::IRT.informationCurves(model_out_eip,
                                               theta = true_theta)
    #names(info_out_eip)[[i]] <- unique(max_temp$eip_name)[


  # Summary
  info_summary_eip <- NULL


  info_summary_eip <- data.frame(info_test = mean(info_out_eip$test_info_curve),


                       eip_name = unique(max_temp$eip_name),
                       item = paste(colnames(out_eip), collapse = ", "))

  info_summary_eip <-  rbind(info_summary_eip,
                             data.frame(info_test = (info_start),
                                        eip_name = "all",
                                        item = "all"))
  info_summary_eip$selection <- "EIP"
  eip_results = list(item_stf = selected_eip,
                     summary = info_summary_eip,
                     info_stf = info_out_eip)
  return(eip_results)
}
