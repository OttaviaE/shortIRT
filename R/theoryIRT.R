#' Select items for the short forms using only item and respondent parameters
#'
#'
#' @param a vector of discrimination parameters
#' @param b vector of difficulty parameters
#' @param strategy select the strategy for selecting the items, \code{smart}, \code{cluster}, \code{guided}
#' @param true_theta respondents' theta
#' @param theta_target number of items for the short form (it defines the theta target for the \code{cluster} and \code{guided} strategies)
#'
#' @return a list
#' @export
#'
#' @examples
theoryIRT = function(a = stats::runif(10, .40, 2),
                     b = stats::runif(10, -3, 3),
                     strategy =  c("smart", "cluster", "guided"),
                     true_theta = stats::rnorm(1000),
                     theta_target = 2) {
  strategy <- match.arg(strategy)

  if (length(a) != length(b)) {
    stop("Discrimination and difficulty parameters must have the same length")
  } else {
    item = data.frame(a, b)
  }

  if (length(theta_target) >=  (nrow(item))) {
    stop("The number of theta target must be lower than the number of items")
  }

  # latent trait divided in clusters
  if (strategy == "cluster") {
    # define theta target
    theta_target = stats::kmeans(matrix(true_theta, ncol = 1),
                   centers = theta_target)
    theta_target = (theta_target$centers)
    name = "cluster"
    # compute information for each item for each theta target (cluster and guided strategies)
    p = data.frame(matrix(nrow = nrow(item),
                          ncol = length(theta_target)))
    q = data.frame(matrix(nrow = nrow(item),
                          ncol = length(theta_target)))
    ii = data.frame(matrix(nrow = nrow(item),
                           ncol = length(theta_target)))
    theta_target = theta_target[order(theta_target)]
    colnames(ii) = theta_target
    rownames(ii) = paste0("item", 1:nrow(item))
    temp = NULL
    for (i in 1:nrow(item)) {
      temp = item[i, ]
      for (j in 1:length(theta_target)) {
        p[i, j] =  exp(temp$a *(theta_target[j] - temp$b))/(1 + exp(temp$a *(theta_target[j] - temp$b)))
        q[i, j] = 1-(exp(temp$a *(theta_target[j] - temp$b))/(1 + exp(temp$a *(theta_target[j] - temp$b))))

        ii[i,j] = temp$a^2 * p[i,j] * q[i,j]
      }
    }

    ii$item =rownames(ii)
    ii = ii[, c(ncol(ii), 1:(ncol(ii)-1))]

    ii_long = stats::reshape(ii,
                             idvar = "item",
                             direction = "long",
                             varying = list(2:ncol(ii)),
                             v.names = "info",
                             timevar = "theta_target",
                             times = names(ii)[-1])


    max_data = stats::aggregate(info ~ item + theta_target,
                                data = ii_long, max)

    max_data =  max_data[order(as.numeric(max_data$theta_target)), ]

    temp = NULL
    max_info = NULL


    for(i in 1:length(unique(max_data$theta_target))) {
      temp1 = max_data[which(max_data$info == max(max_data$info)), ]
      max_data = max_data[which(max_data$item != temp1$item & max_data$theta_target != temp1$theta_target), ]
      max_info = rbind(max_info, temp1)
    }
  } # latent trait divided in intervals
  else if (strategy == "guided") {
    name = "guided"
    theta_target = seq(min(true_theta),max(true_theta),
                     length.out = theta_target)
    # compute information for each item for each theta target (cluster and guided strategies)
    p = data.frame(matrix(nrow = nrow(item),
                          ncol = length(theta_target)))
    q = data.frame(matrix(nrow = nrow(item),
                          ncol = length(theta_target)))
    ii = data.frame(matrix(nrow = nrow(item),
                           ncol = length(theta_target)))
    theta_target = theta_target[order(theta_target)]
    colnames(ii) = theta_target
    rownames(ii) = paste0("item", 1:nrow(item))
    temp = NULL
    for (i in 1:nrow(item)) {
      temp = item[i, ]
      for (j in 1:length(theta_target)) {
        p[i, j] =  exp(temp$a *(theta_target[j] - temp$b))/(1 + exp(temp$a *(theta_target[j] - temp$b)))
        q[i, j] = 1-(exp(temp$a *(theta_target[j] - temp$b))/(1 + exp(temp$a *(theta_target[j] - temp$b))))

        ii[i,j] = temp$a^2 * p[i,j] *q[i,j]
      }
    }

    ii$item =rownames(ii)
    ii = ii[, c(ncol(ii), 1:(ncol(ii)-1))]

    ii_long = stats::reshape(ii,
                             idvar = "item",
                             direction = "long",
                             varying = list(2:ncol(ii)),
                             v.names = "info",
                             timevar = "theta_target",
                             times = names(ii)[-1])


    max_data = stats::aggregate(info ~ item + theta_target,
                                data = ii_long, max)

    max_data =  max_data[order(as.numeric(max_data$theta_target)), ]

    temp = NULL
    max_info = NULL


    for(i in 1:length(unique(max_data$theta_target))) {
      temp1 = max_data[which(max_data$info == max(max_data$info)), ]
      max_data = max_data[which(max_data$item != temp1$item & max_data$theta_target != temp1$theta_target), ]
      max_info = rbind(max_info, temp1)
    }
  } # latent trait not divided
  else if (strategy == "smart") {
    name = "smart"
    theta_target = theta_target
    # first compute the information of all items across the true theta
    p_all = data.frame(matrix(nrow = nrow(item),
                              ncol = length(true_theta)))
    q_all = data.frame(matrix(nrow = nrow(item),
                              ncol = length(true_theta)))
    ii_all = data.frame(matrix(nrow = nrow(item),
                               ncol = length(true_theta)))
    colnames(ii_all) = true_theta
    true_theta = true_theta[order(true_theta)]
    for (i in 1:nrow(item)) {
      temp = item[i, ]
      for (j in 1:length(true_theta)) {
        p_all[i, j] =  exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b)))
        q_all[i, j] = 1-(exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b))))

        ii_all[i,j] = temp$a^2 * p_all[i,j] *q_all[i,j]
      }
    }

    # ii_ll has the information provided by each item (rows) at each level of the latent trait (columns)
    # reshape data set

    ii_all$item = paste("item", 1:nrow(ii_all), sep = "")

    ii_all = ii_all[, c(ncol(ii_all), 1:(ncol(ii_all)-1))]

    ii_all_long = stats::reshape(ii_all,
                          idvar = "item",
                          direction = "long",
                          varying = list(2:ncol(ii_all)),
                          v.names = "value",
                          timevar = "theta_target",
                          times = names(ii_all)[-1])

    # now get the desired n items providing the max information on the latent trait,
    # regardless of the specific point of the latent trait
    temp = NULL
    max_data = ii_all_long
    max_info = NULL

    for (i in 1:theta_target) {
      temp = max_data[which(max_data$value == max(max_data$value)), ]
      max_data = max_data[which(max_data$item != temp$item), ]
      max_info = rbind(max_info, temp)
    }

  }


  # compute information for the reduced number of item

  item$item = paste("item", 1:nrow(item), sep = "")
  small_item = item[item$item %in% max_info$item, ]
  temp = NULL
  p_all_short = data.frame(matrix(nrow = nrow(small_item),
                                  ncol = length(true_theta)))
  q_all_short = data.frame(matrix(nrow = nrow(small_item),
                                  ncol = length(true_theta)))
  ii_all_short = data.frame(matrix(nrow = nrow(small_item),
                                   ncol = length(true_theta)))
  colnames(ii_all_short) = true_theta
  true_theta = true_theta[order(true_theta)]
  for (i in 1:nrow(small_item)) {
    temp = small_item[i, ]
    for (j in 1:length(true_theta)) {
      p_all_short[i, j] =  exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b)))
      q_all_short[i, j] = 1-(exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b))))

      ii_all_short[i,j] = temp$a^2 * p_all_short[i,j] *q_all_short[i,j]
    }
  }

  # test information of the short form -----
  info_short = mean(colSums(ii_all_short))

  p_all = data.frame(matrix(nrow = nrow(item),
                            ncol = length(true_theta)))
  q_all = data.frame(matrix(nrow = nrow(item),
                            ncol = length(true_theta)))
  ii_all = data.frame(matrix(nrow = nrow(item),
                             ncol = length(true_theta)))
  colnames(ii_all) = true_theta
  true_theta = true_theta[order(true_theta)]
  for (i in 1:nrow(item)) {
    temp = item[i, ]
    for (j in 1:length(true_theta)) {
      p_all[i, j] =  exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b)))
      q_all[i, j] = 1-(exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b))))

      ii_all[i,j] = temp$a^2 * p_all[i,j] *q_all[i,j]
    }
  }

  # test information of the entire test -----

  info_total = mean(colSums(ii_all))

  rownames(max_info) = NULL

  info_data_short =data.frame(theta = as.numeric(colnames(ii_all_short)),
                         info = colSums(ii_all_short),
                         type = "short",
                         row.names = NULL)
  info_data_all =data.frame(theta = as.numeric(colnames(ii_all)),
                         info = colSums(ii_all),
                         type = "all",
                         row.names = NULL)

  info_data = rbind(info_data_short, info_data_all)
  max_info = max_info[order(max_info$theta_target), ]
  results = list(item = max_info,
                 info_start = info_total,
                 info_short = info_short,
                 info_data = info_data)
  class(results) = append(class(results), c("shortForm",name))
  return(results)
}
