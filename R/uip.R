#' uip
#'
#' Create a Short Test Form (STF) using the theta-target procedure based on the unequal segmentation of the latent trait
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
#' @importFrom dplyr distinct
#'
#'
#' @return A list of length 3
#' @export
#'
#' @examples
uip <- function(data,
                fixed.b = NULL,
                fixed.a = NULL,
                seed = 999,
                true_theta = NULL,
                num_item = NULL,
                theta_targets = NULL) {
  if (is.null(num_item) & is.null(theta_targets)) {
    stop("You must specify either a number of items for the STFs or the theta targets!")
  }
  if(is.null(fixed.a) & is.null(fixed.b)) {
    start_model <- TAM::tam.mml(data, verbose = FALSE)
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


  if (!is.null(true_theta)) {
    if (length(true_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    true_theta <- start_model$person$EAP
  }
  if (!is.null(theta_targets)) {
    cluster <- theta_targets
  } else {
    num_clusters <- num_item
    theta_mat <- matrix(true_theta, ncol = 1)
    cluster <- kmeans(theta_mat,
                      centers = num_clusters)
    cluster <- cluster$centers[,1]
  }

  cluster_data   <- NULL
  info_data_cluster <- NULL

  value_cluster <- cluster
    for(i in 1:length(lab_item)) {
      for(j in 1:length(value_cluster)) {

        temp_cluster_data   <- data.frame(theta_target = TAM::IRT.informationCurves(start_model,
                                                                               theta = value_cluster[j],
                                                                               iIndex = lab_item[i])$theta,

                                          item_info = colSums(TAM::IRT.informationCurves(start_model,
                                                                                    theta = value_cluster[j],
                                                                                    iIndex = lab_item[i])$info_curves_item),
                                          item = lab_item[i],
                                          num_item = paste("STF-",
                                                           num_item, sep = ""))

        info_data_cluster <- rbind(info_data_cluster, temp_cluster_data  )
      }
    }


  if (length(unique(cluster)) > 1) {
    temp_data_cluster <- NULL
    temp_maxcluster <- NULL
    temp <- NULL
    max_temp_cluster <- NULL

    temp_maxcluster <- aggregate(item_info ~ item + theta_target,
                                 data = info_data_cluster, max)
    temp_maxcluster$cluster_name <- unique(info_data_cluster$num_item)
    for (i in 1:length(unique(temp_maxcluster$theta_target))) {
      temp <- temp_maxcluster[which(temp_maxcluster$item_info == max(temp_maxcluster$item_info)), ]
      temp_maxcluster <- temp_maxcluster[which(temp_maxcluster$item != temp$item &
                                                 temp_maxcluster$theta_target != temp$theta_target), ]
      max_temp_cluster <-rbind(max_temp_cluster, temp)

    }
  } else {
    info_data_cluster <- info_data_cluster[order(info_data_cluster$item_info, decreasing = TRUE),]
    max_temp_cluster <- dplyr::distinct(info_data_cluster)
    max_temp_cluster <- max_temp_cluster[1:length(theta_targets), ]
    max_temp_cluster$cluster_name <- unique(info_data_cluster$num_item)
  }


  selected_uip = max_temp_cluster[order(max_temp_cluster$theta_target), ]

  # given the number(s) of items in num_item, filter out the selected ones from the
  # full-length test, estimate the model on the resulting short form(s), and
  # compute the IIF and TIF

    out_cluster <- data[, c(max_temp_cluster[max_temp_cluster$cluster_name %in% unique(max_temp_cluster$cluster_name),
                                                  "item"])]
    model_out_cluster <- TAM::tam.mml(out_cluster,
                                      xsi.fixed = cbind(1:ncol(out_cluster),
                                                        b_true[as.integer(gsub("I00|I0|I", '',
                                                                               colnames(out_cluster))), 2]),
                                      B= array(c(rep(0, ncol(out_cluster)),
                                                 a_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                             colnames(out_cluster)))]),
                                               c(ncol(out_cluster),2,1),
                                               dimnames = list(colnames(out_cluster),
                                                               c("Cat0", "Cat1"),
                                                               "Dim01")),
                                 verbose = FALSE)
    info_out_cluster <- TAM::IRT.informationCurves(model_out_cluster,
                                                   theta = true_theta)

  # summary

    info_summary_cluster <- data.frame(info_test = mean(info_out_cluster$test_info_curve),
                       cluster_name = unique(max_temp_cluster$cluster_name),
                       item = paste(colnames(out_cluster), collapse = ","))



  info_summary_cluster <-  rbind(info_summary_cluster,
                                 data.frame(info_test = info_start,
                                            cluster_name = "all",
                                            item = "all"))
  info_summary_cluster$selection <- "UIP"

  uip_results = list(item_stf = selected_uip,
                     summary = info_summary_cluster,
                     info_stf = info_out_cluster)
  return(uip_results)
}
