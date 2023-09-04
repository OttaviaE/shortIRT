bp <- function(data,
                fixed.b = NULL,
                fixed.a = NULL,
                seed = 999,
                true_theta = NULL,
                num_item = NULL) {
  if (is.null(num_item)) {
    stop("You must specify a number of items for the STFs")
  }



  if(is.null(fixed.a) & is.null(fixed.b)) {
    start_model <- TAM::tam.mml(data)
  } else {
    b_true <- matrix(cbind(1:length(fixed.b),
                           fixed.b),
                     ncol = 2)
    a_true <- array(c(rep(0, length(fixed.a)), fixed.a),
                    c(length(fixed.a),2,1),
                    dimnames = list(paste0("I", 1:length(fixed.a)),
                                    c("Cat0", "Cat1"),
                                    "Dim01"))

    start_model = TAM::tam.mml(resp=data, xsi.fixed = b_true, B = a_true)
  }

  if (!is.null(true_theta)) {
    if (length(true_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    true_theta <- start_model$EAP
  }

  info_start <- mean(IRT.informationCurves(start_model,
                                           theta = true_theta)$test_info_curve)

  lab_item <- 1:ncol(data)
  num_item <- num_item


  data_info_bp <- data.frame(items = 1:(ncol(data)),
                             item_info = numeric((ncol(data))))

  for (i in 1:nrow(data_info_bp)) {
    data_info_bp[i, "item_info"] <- mean(IRT.informationCurves(start_model,
                                                          theta = true_theta,
                                                          iIndex = lab_item[i])$info_curves_item)

  }

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
    model_out_bp <- tam.mml(out_bp,
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

    info_out_bp <- IRT.informationCurves(model_out_bp,
                                              theta = true_theta)


  # Summary
  info_summary_bp <- NULL
  temp <- NULL

  info_summary_bp <- data.frame(info_test = mean(info_out_bp$test_info_curve),
                       bp_name = paste0("number", num_item),
                       item = paste(colnames(out_bp), collapse = ","))

  info_summary_bp <-  rbind(info_summary_bp,
                            data.frame(info_test = (info_start),
                                       bp_name = "all",
                                       item = "all"))
  info_summary_bp$selection <- "BP"


  bp_results = list(item_stf = selected_bp,
                     summary = info_summary_bp,
                     info_stf = info_out_bp)
  return(bp_results)
}
