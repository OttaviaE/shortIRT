#' uip
#'
#' Create a Short Test Form (STF) using the theta-target procedure based on the unequal segmentation of the latent trait
#'
#' @param data a subject x item matrix
#' @param item_par Two column matrix with the item parameters. The first column must contain the difficulty parameters, the second column must contain the discrimination parameters.
#' @param seed a random seed (default is 999)
#' @param true_theta a vector with length equal to the number of rows in data with the true theta values of the subjects. If empty, the theta values will be estimated from the data.
#' @param num_item the number of item to included in the short test form
#'
#' @import TAM
#' @import sirt
#' @import dplyr
#'
#'
#' @return A list of length 3:
#'          - item_stf: data frame with as many rows as the the number of items to be included in the STF.
#'          - summary: contains the list of items included in the STF and the total information, along with the information of the full length test
#'          - info_stf: contains the information for each level of the latent trait. used for plotting
#' @export
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
#' stf <- uip(data, true_theta = true_theta, item_par = parameters, num_item = 5)
#' # check the obtained short test form
#' stf$item_stf
#' # check the comparison between the short test form and the full-length test
#' stf$summary
#' }
uip <- function(data,
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


  info_start <- mean(TAM::IRT.informationCurves(start_model,
                                           theta = true_theta)$test_info_curve)
  info_start_theta <- TAM::IRT.informationCurves(start_model,
                                                 theta = true_theta)

  lab_item <- 1:ncol(data)
  if (!is.null(true_theta)) {
    if (length(true_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    true_theta <- start_model$person$EAP
  }
      num_clusters <- num_item
    theta_mat <- matrix(true_theta, ncol = 1)
    cluster <- stats::kmeans(theta_mat,
                      centers = num_clusters)
    cluster <- cluster$centers[,1]

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


    temp_data_cluster <- NULL
    temp_maxcluster <- NULL
    temp <- NULL
    max_temp_cluster <- NULL

    temp_maxcluster <- stats::aggregate(item_info ~ item + theta_target,
                                 data = info_data_cluster, max)
    temp_maxcluster$stf_length <- unique(info_data_cluster$num_item)
    for (i in 1:length(unique(temp_maxcluster$theta_target))) {
      temp <- temp_maxcluster[which(temp_maxcluster$item_info == max(temp_maxcluster$item_info)), ]
      temp_maxcluster <- temp_maxcluster[which(temp_maxcluster$item != temp$item &
                                                 temp_maxcluster$theta_target != temp$theta_target), ]
      max_temp_cluster <-rbind(max_temp_cluster, temp)
    }


  selected_uip = max_temp_cluster[order(max_temp_cluster$theta_target), ]

  # given the number(s) of items in num_item, filter out the selected ones from the
  # full-length test, estimate the model on the resulting short form(s), and
  # compute the IIF and TIF

    out_cluster <- data[, c(max_temp_cluster[max_temp_cluster$stf_length %in% unique(max_temp_cluster$stf_length),
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
                       stf_length = unique(max_temp_cluster$stf_length),
                       item = paste(colnames(out_cluster), collapse = ","))



  info_summary_cluster <-  rbind(info_summary_cluster,
                                 data.frame(info_test = info_start,
                                            stf_length = "all",
                                            item = "all"))
  info_summary_cluster$selection <- "UIP"

  uip_results = list(item_stf = selected_uip,
                     summary = info_summary_cluster,
                     info_stf = info_out_cluster,
                     info_start = info_start_theta)
  return(uip_results)
}