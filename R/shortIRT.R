#' Select item for the short forms using observed data
#'
#'
#' @param data data frame with observed responses
#' @param num_item number of desired items for the short version
#' @param strategy which strategy for selecting the items?
#' @param fixed.a vector of fixed discrimination parameters
#' @param fixed.b vector of fixed difficulty parameters
#' @param true_theta vector of respondent parameters
#'
#' @importFrom  TAM tam.mml tam.mml.2pl IRT.informationCurves
#' @import tidyr
#' @import stats
#' @return a data set
#' @export
#'
#' @examples
shortIRT <- function(data,
                     num_item = NULL,
                     strategy = c("EIP", "UIP", "typical"),
                     fixed.b = NULL,
                     fixed.a = NULL,
                     true_theta = NULL, seed = 999) {
  seed <- seed
  set.seed(seed)
  strategy <- match.arg(strategy)

  if (!is.null(fixed.b) & is.null(fixed.a)) {
    if (is.data.frame(fixed.b) | is.matrix(fixed.b) | is.factor(fixed.b)) {
      stop("STOP! item difficulties must be a numeric vector")
      # create fixed difficulties
    } else {
      diff_true <- matrix(cbind(1:length(fixed.b),
                                fixed.b),  ncol = 2)
      # estimate starting model
      start_model = TAM::tam.mml(resp=data, xsi.fixed = matrix(cbind(1:length(fixed.b),
                                                                fixed.b),  ncol = 2))
      }
  } else if (is.null(fixed.b) & !is.null(fixed.a)) {
    if (is.data.frame(fixed.a) | is.matrix(fixed.a) | is.factor(fixed.a)) {
      stop("STOP! item disciminations must be a numeric vector")

    } else {
      # create fixed disciminations
      discr_true = array(c(rep(0, length(fixed.a)), fixed.a),
                         c(length(fixed.a),2,1),
                         dimnames = list(paste0("I", 1:length(fixed.a)),
                                         c("Cat0", "Cat1"),
                                         "Dim01"))
      # estimate starting model
      start_model = TAM::tam.mml(resp=data,  xsi.fixed = diff_true,
                                 B = discr_true)
    }
  } else if (!is.null(fixed.b) & !is.null(fixed.a)) {
    if (is.data.frame(fixed.a) | is.matrix(fixed.a) | is.factor(fixed.a) |
        is.data.frame(fixed.b) | is.matrix(fixed.b) | is.factor(fixed.b)) {
      stop("STOP! item difficulties and disciminations must be numeric vectors")
    } else {
      # create fixed difficulties and discriminations
      diff_true <- matrix(cbind(1:length(fixed.b), fixed.b),  ncol = 2)
      discr_true = array(c(rep(0, length(fixed.a)), fixed.a),
                         c(length(fixed.a),2,1),
                         dimnames = list(paste0("I", 1:length(fixed.a)),
                                         c("Cat0", "Cat1"),
                                         "Dim01"))
      # estimate starting model
      start_model = TAM::tam.mml(resp=data, xsi.fixed = diff_true,
                            B = discr_true)
    }
  } else {
      start_model = TAM::tam.mml.2pl(data)
  }
  # set starting theta, whether observed or not
  if (is.null(true_theta)) {
    theta = start_model$person$EAP
  } else {
    if (length(true_theta) != nrow(data)) {
      stop("theta and respondenta must correspond")
    } else {
      theta = true_theta
    }
  }

  if (strategy == "UIP") {
    lab_item <- 1:ncol(data)
    num_clusters <- num_item
    # la divisione in cluster è fatta sulla base dei theta simulati
    theta_mat <- matrix(theta, ncol = 1)
    info_start <- mean(TAM::IRT.informationCurves(start_model,
                                             theta = seq(-3,3,
                                                         length = 1000))$test_info_curve)
    # generate cluster
    cluster <- stats::kmeans(theta_mat, centers = num_clusters)

    # calcola l'informatività per ogni theta per ogni item

    temp_k <- list()
    temp_k_data   <- NULL
    info_data_k <- NULL

    value_k = cluster$centers[order(cluster$centers, decreasing = FALSE)]


    for(i in 1:length(lab_item)) {
      for(j in 1:length(value_k)) {

        temp_k_data   <- data.frame(theta_target = TAM::IRT.informationCurves(start_model,
                                                                         theta = value_k[j],
                                                                         iIndex = lab_item[i])$theta,
                                    test_info = mean(TAM::IRT.informationCurves(start_model,
                                                                           theta = value_k[j],
                                                                           iIndex = lab_item[i])$test_info_curve),
                                    item = lab_item[i],
                                    num_item = paste("number", length(value_k), sep = ""))

        info_data_k <- rbind(info_data_k, temp_k_data  )
      } }
    # select item for each theta target with max information

    temp_data_k <- NULL
    temp_maxcluster <- NULL
    temp <- NULL
    max_temp_k <- NULL

    for (i in 1:length(unique(info_data_k$num_item))){
      temp_data <- info_data_k[info_data_k$num_item %in% unique(info_data_k$num_item)[i], ]
      temp_maxcluster <- stats::aggregate(test_info ~ item + theta_target,
                                   data = temp_data, max)
      temp_maxcluster$cluster_name <- unique(temp_data$num_item)

      for (j in 1:length(unique(temp_maxcluster$theta_target))) {
        temp <- temp_maxcluster[which(temp_maxcluster$test_info == max(temp_maxcluster$test_info)), ]
        temp_maxcluster <- temp_maxcluster[which(temp_maxcluster$item != temp$item &
                                                   temp_maxcluster$theta_target != temp$theta_target), ]
        max_temp_k <-rbind(max_temp_k, temp)

      }
    }

    # take out from the dataset the items with the least information and estimate the model
    # again
    # WARNING: fixed vs free parameters
    out_cluster <- data[, c(max_temp_k[max_temp_k$cluster_name %in% unique(max_temp_k$cluster_name),
                                             "item"])]

    if (!is.null(fixed.b) & is.null(fixed.a)) {
      model_out_cluster <- TAM::tam.mml.2pl(out_cluster,
                                         xsi.fixed = cbind(1:ncol(out_cluster),
                                                           diff_true[tidyr::extract_numeric(colnames(out_cluster)), 2]))
    } else if (is.null(fixed.b) & !is.null(fixed.a)) {
      model_out_cluster <- TAM::tam.mml(out_cluster,

                                         B= array(c(rep(0, ncol(out_cluster)),
                                                    discr_true[,2,][tidyr::extract_numeric(colnames(out_cluster))]),
                                                  c(ncol(out_cluster),2,1),
                                                  dimnames = list(colnames(out_cluster),
                                                                  c("Cat0", "Cat1"),
                                                                  "Dim01")))
    } else if (!is.null(fixed.b) & !is.null(fixed.a)) {
      # set both discrimination and difficulties parameters
      model_out_cluster <- TAM::tam.mml(out_cluster,
                                         xsi.fixed = cbind(1:ncol(out_cluster),
                                                           diff_true[tidyr::extract_numeric(colnames(out_cluster)), 2]),
                                         B= array(c(rep(0, ncol(out_cluster)),
                                                    discr_true[,2,][tidyr::extract_numeric(colnames(out_cluster))]),
                                                  c(ncol(out_cluster),2,1),
                                                  dimnames = list(colnames(out_cluster),
                                                                  c("Cat0", "Cat1"),
                                                                  "Dim01")))

    } else {
      model_out_cluster <- TAM::tam.mml.2pl(out_cluster)
    }
    info_out_cluster <- TAM::IRT.informationCurves(model_out_cluster,
                                              theta = seq(-3, 3, length = 1000))

    info_summary_cluster <- data.frame(info_test = mean(info_out_cluster$test_info_curve),
                                       cluster_name = unique(max_temp_k$cluster_name),
                                       item = paste(colnames(out_cluster), collapse = ","))


    info_summary_cluster$rel <- 1 - (1/sqrt(info_summary_cluster$info_test))^2

    info_summary_cluster <-  rbind(info_summary_cluster,
                                   data.frame(info_test = info_start,
                                              cluster_name = "all",
                                              item = "all",
                                              rel = 1 - (1/sqrt(info_start))^2))
    cluster_results <- list(start_model = start_model,
                            reduced_model = model_out_cluster,
                            info_summary_cluster = info_summary_cluster,
                            theta_target = value_k)
  }
  return(cluster_results)
 }

