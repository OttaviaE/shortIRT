#' Item Information Function (single item, \eqn{I_i(\theta)})
#'
#' Compute the item information function \eqn{I_i(\theta)} for a single dichotomous
#' or polytoumous item under either the 4-PL model (dichotomous item) or  the Generalized Partial Credit model (polytomous item).
#' Specific models (e.g., 3-PL, 2-PL, 1-PL, or PCM, Rating Scale) are obtained by imposing
#' constraints on the item parameters.
#' See \code{Details}.
#'
#' @param item_pars \code{data.frame} with number of rows equal to the number of items.
#'    For dichotomous items, the dataframe must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the dataframe has \eqn{2K} columns, where \eqn{K} is the number of thresholds of the items (number of response categories \eqn{- 1}). The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters
#' @param theta \code{numeric} vector of latent trait values. Default is a vector of a thousand values ranging from -5 to +5
#' @param K \code{integer} defining the number of thresholds for  the categories of the polytoumous items (i.e., number of response categories minus 1). Default is \code{NULL} (assumes dichotomous items).
#' @details
#' Let \eqn{P(\theta)} denote the  probability of a correct response \eqn{x_{pi} = 1} for person \eqn{p} (with latent trait level defined as \eqn{\theta_p}) on item \eqn{i} under the four-parameter logistic
#' (4-PL; Barton & Lord, 1981) model is defined as:
#'
#' \deqn{
#' P(\theta) =
#' c_i + \frac{e_i - c_i}{1 + \exp\left[-a_i(\theta_p - b_i)\right]}
#' }
#'
#' where \eqn{a_i} is the discrimination parameter,
#' \eqn{b_i} is the difficulty parameter (or location of item \eqn{i} on the latent trait),
#' \eqn{c_i} is the lower asymptote (pseudo-guessing probability),
#' and \eqn{e_i} is the upper asymptote (inattention/slip). By constraining \eqn{e_i = 1}, \eqn{c_i = 0}, and \eqn{a_i=1} \eqn{\forall i}, the probability
#' is computed according to the 3-PL (Lord, 1980), 2-PL (Birnbaum, 1968) and 1-PL or the Rasch model (Rasch, 1960), respectively.
#'
#' Let \eqn{Q(\theta) = 1 - P(\theta)}, the information function of item \eqn{i} is computed as:
#'
#' \deqn{
#' I_i(\theta) =
#' \frac{a_i^2 \left[P(\theta) - c_i\right]^2 \left[e_i - P(\theta)\right]^2}
#' {(e_i - c_i)^2 \, P(\theta) \, Q(\theta)}
#' }
#'
#' According to the Generalized Partial Credit Model (GPCM; Muraki, 1997), for a polytomous item with \eqn{K} thresholds separating the \eqn{K + 1} categories, the probability of category \eqn{k} is defined as:
#' \deqn{
#' P(Y = k \mid \theta) =
#' \frac{\exp\left( \sum_{k=1}^K a_k (\theta - b_k) \right)}
#' {\sum_{j=0}^K \exp\left( \sum_{k=1}^K a_k (\theta - b_k) \right)}
#' }
#'
#' where \eqn{a_k} and \eqn{b_k} are the discrimination and location parameters associate with each threshold \eqn{k}. If \eqn{a_k = 1, \, \forall k}, the Partial Credit Model (PCM, Muraki, 1992) is obtained.
#'
#' The item information is computed as:
#' \deqn{
#' I_i(\theta) = \sum_{k=0}^K \frac{[P'_k(\theta)]^2}{P_k(\theta)}
#' }
#'
#' @references Barton, M. A., & Lord, F. M. (1981). An upper asymptote for the
#' three-parameter logistic item-response model. ETS Research Report
#' Series, 1981(1), i–8. Princeton, NJ: Educational Testing Service.
#'
#' Birnbaum, A. (1968). Some latent trait models and their use in inferring
#' an examinee's ability. In F. M. Lord & M. R. Novick (Eds.),
#' Statistical theories of mental test scores (pp. 397–479).
#' Reading, MA: Addison-Wesley.
#'
#' Lord, F. M. (1980). Applications of item response theory to practical
#' testing problems. Hillsdale, NJ: Lawrence Erlbaum Associates.
#'
#' Muraki, G. (1992). A generalized partial credit model: Application of an EM algorithm.
#' Psychometrika, 57(2), 159–176.
#'
#' Muraki, G. (1997). A generalized partial credit model with step discrimination.
#' Journal of Educational Measurement, 34(2), 115–127.
#'
#' Rasch, G. (1960). Probabilistic models for some intelligence and attainment
#' tests. Copenhagen, Denmark: Danish Institute for Educational Research.
#'
#' @returns A numeric vector of length equal to \code{theta}, which contains the item information function for a single item with respect to the values specified in \code{theta}
#' @export
#'
#' @examples
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Create a sequence of latent trait values
#' # spanning the ability continuum
#' theta <- seq(-4, 4, length.out = 200)
#'
#' # Define parameters for a dichotomous item
#' # b = difficulty
#' # a = discrimination
#' # c = lower asymptote (guessing)
#' # e = upper asymptote
#' item_par <- data.frame(
#'   b = 0,
#'   a = 1.5,
#'   c = .10,
#'   e = .98
#' )
#'
#' # Compute item information function (IIF)
#' # across the theta continuum
#' info_dichotomous <- i_info(item_par, theta = theta)
#'
#' # Define parameters for a 4-category item
#' # (K = 3 thresholds / category transitions)
#' # a's = category discrimination parameters
#' # b's = threshold/location parameters
#' item_pars <- data.frame(
#'   a1 = 1.2,
#'   a2 = 1.0,
#'   a3 = 0.8,
#'   b1 = -1.0,
#'   b2 = 0.0,
#'   b3 = 1.2
#' )
#'
#' # Compute item information for the polytomous item
#' # across the theta continuum
#' info <- i_info(item_pars, theta = theta, K = 3)
i_info <- function(item_pars,
                   theta = seq(-5,5,length.out=1000), K = NULL){
  if (is.null(K)) {
    if (length(unique(gsub("[0-9]", "", colnames(item_pars)))) == 2) {
      stop("You forgot to specifiy the number of thresholds for your items!")
    }
    a <- item_pars$a
    e <- item_pars$e
    c <- item_pars$c
    P <- IRT(theta, b = item_pars$b, a = a, e = e, c=c)
    Q <- 1 - P
    num <- (a^2)*((P-c)^2)*((e-P)^2)
    den <- ((e-c)^2)*P*Q
    info <- num/den
  } else {
    if (ncol(item_pars) != 2 * K)
      stop("item_pars must have exactly 2*K columns.")

    # extract parameters
    a <- as.numeric(item_pars[1, 1:K])
    b <- as.numeric(item_pars[1, (K + 1):(2 * K)])

    n_theta <- length(theta)

    # linear predictors for categories 0,...,K
    eta <- sapply(0:K, function(k) {
      if (k == 0) {
        rep(0, n_theta)
      } else {
        rowSums(
          sapply(1:k, function(s) a[s] * (theta - b[s]))
        )
      }
    })

    # category probabilities
    exp_eta <- exp(eta)
    P <- exp_eta / rowSums(exp_eta)

    # derivatives of eta with respect to theta
    deta <- sapply(0:K, function(k) {
      if (k == 0) {
        rep(0, n_theta)
      } else {
        rep(sum(a[1:k]), n_theta)
      }
    })

    # derivatives of probabilities
    dP <- P * (deta - rowSums(P * deta))

    # item information
    info <- rowSums((dP^2) / P, na.rm = TRUE)
  }

  return(info)
}

#' Item Information Functions (multiple items, \eqn{I_i(\theta)})
#'
#' Computes the item information functions for multiple dichotomous or polytomous items
#'
#' @param item_pars \code{data.frame} with number of rows equal to the number of items.
#'    For dichotomous items, the dataframe must have 4 columns, one for each of the item parameters. The columns must be named "a", "b", "c", "e" and must contain the respective IRT parameters, namely discrimination \eqn{a_i}, location \eqn{b_i}, pseudo-guessing \eqn{c_i}, and upper asymptote \eqn{e_i}.
#'    For polytomous items, the dataframe has \eqn{2K} columns, where \eqn{K} is the number of thresholds of the items (number of response categories \eqn{- 1}). The first \eqn{K} columns correspond to step
#'   discrimination parameters \eqn{a_1, \dots, a_K} (must be named "a"), and the last \eqn{K}
#'   columns correspond to step difficulty (threshold) parameters
#' @param theta \code{numeric} vector of latent trait values. Default is a vector of A thousand values ranging from -5 to +5
#' @param K \code{integer} defining the number of thresholds for  the categories of the polytoumous items (i.e., number of response categories minus 1). Default is \code{NULL} (assumes dichotomous items).
#' @details
#' Let \eqn{P(\theta)} denote the  probability of a correct response \eqn{x_{pi} = 1} for person \eqn{p} (with latent trait level defined as \eqn{\theta_p}) on item \eqn{i} under the four-parameter logistic
#' (4-PL; Barton & Lord, 1981) model is defined as:
#'
#' \deqn{
#' P(\theta) =
#' c_i + \frac{e_i - c_i}{1 + \exp\left[-a_i(\theta_p - b_i)\right]}
#' }
#'
#' where \eqn{a_i} is the discrimination parameter,
#' \eqn{b_i} is the difficulty parameter (or location of item \eqn{i} on the latent trait),
#' \eqn{c_i} is the lower asymptote (pseudo-guessing probability),
#' and \eqn{e_i} is the upper asymptote (inattention/slip). By constraining \eqn{e_i = 1}, \eqn{c_i = 0}, and \eqn{a_i=1} \eqn{\forall i}, the probability
#' is computed according to the 3-PL (Lord, 1980), 2-PL (Birnbaum, 1968) and 1-PL or the Rasch model (Rasch, 1960), respectively.
#'
#' Let \eqn{Q(\theta) = 1 - P(\theta)}, the information function of item \eqn{i} is computed as:
#'
#' \deqn{
#' I_i(\theta) =
#' \frac{a_i^2 \left[P(\theta) - c_i\right]^2 \left[e_i - P(\theta)\right]^2}
#' {(e_i - c_i)^2 \, P(\theta) \, Q(\theta)}
#' }
#'
#' According to the Generalized Partial Credit Model (GPCM; Muraki, 1997), for a polytomous item with \eqn{K} thresholds separating the \eqn{K + 1} categories, the probability of category \eqn{k} is defined as:
#' \deqn{
#' P(Y = k \mid \theta) =
#' \frac{\exp\left( \sum_{k=1}^K a_k (\theta - b_k) \right)}
#' {\sum_{j=0}^K \exp\left( \sum_{k=1}^K a_k (\theta - b_k) \right)}
#' }
#'
#' where \eqn{a_k} and \eqn{b_k} are the discrimination and location parameters associate with each threshold \eqn{k}. If \eqn{a_k = 1, \, \forall k}, the Partial Credit Model (PCM, Muraki, 1992) is obtained.
#'
#' The item information is computed as:
#' \deqn{
#' I_i(\theta) = \sum_{k=0}^K \frac{[P'_k(\theta)]^2}{P_k(\theta)}
#' }
#'
#' @references Barton, M. A., & Lord, F. M. (1981). An upper asymptote for the
#' three-parameter logistic item-response model. ETS Research Report
#' Series, 1981(1), i–8. Princeton, NJ: Educational Testing Service.
#'
#' Birnbaum, A. (1968). Some latent trait models and their use in inferring
#' an examinee's ability. In F. M. Lord & M. R. Novick (Eds.),
#' Statistical theories of mental test scores (pp. 397–479).
#' Reading, MA: Addison-Wesley.
#'
#' Lord, F. M. (1980). Applications of item response theory to practical
#' testing problems. Hillsdale, NJ: Lawrence Erlbaum Associates.
#'
#' Muraki, G. (1992). A generalized partial credit model: Application of an EM algorithm.
#' Psychometrika, 57(2), 159–176.
#'
#' Muraki, G. (1997). A generalized partial credit model with step discrimination.
#' Journal of Educational Measurement, 34(2), 115–127.
#'
#' Rasch, G. (1960). Probabilistic models for some intelligence and attainment
#' tests. Copenhagen, Denmark: Danish Institute for Educational Research.
#'
#'
#' @returns A \code{matrix} of class \code{iifs} with number of rows equal to the length of \code{theta} and number of columns equal to the number of items in \code{item_par}
#' @export
#'
#' @examples
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Define parameters for five dichotomous items
#' # b = difficulty parameters
#' # a = discrimination parameters
#' # c = lower asymptote
#' # e = upper asymptote
#' parameters <- data.frame(
#'   b = c(-3, -2, 0, 2, 3),
#'   a = runif(5, 1.2, 1.9),
#'   c = rep(0, 5),
#'   e = rep(1, 5)
#' )
#'
#' # Compute item information functions for
#' # dichotomous items using default theta values
#' infos <- item_info(parameters)
#'
#' # Display the first rows of the information matrix
#' head(infos)
#'
#' # Define parameters for four polytomous items
#' # with four response categories (K = 3 thresholds)
#' item_pars <- data.frame(
#'   matrix(
#'     c(
#'       1.2, 1.0, 0.8,  -1.0,  0.0, 1.2,
#'       0.9, 1.1, 1.3,  -0.5,  0.7, 1.8,
#'       0.5, 1.5, 1.0,  -1.5, -1.0, 0.0,
#'       1.0, 1.0, 1.0,  -1.5,  0.0, 0.5
#'     ),
#'     nrow = 4,
#'     byrow = TRUE
#'   )
#' )
#'
#' # Assign parameter names:
#' # a1-a3 = discrimination parameters
#' # b1-b3 = threshold/location parameters
#' colnames(item_pars) <- paste(
#'   rep(c("a", "b"), each = 3),
#'   1:3,
#'   sep = ""
#' )
#'
#' # Compute item information functions
#' # for the polytomous items
#' info_poly <- item_info(item_pars, K = 3)
#'
#' # Display the first rows of the information matrix
#' head(info_poly)
item_info <- function(item_pars, theta = seq(-5,5,length.out=1000), K = NULL){
  if (is.null(K)) {
    if (length(unique(gsub("[0-9]", "", colnames(item_pars)))) == 2) {
      stop("You forgot to specifiy the number of thresholds for your items!")
    }
  item <- lapply(1:nrow(item_pars), function(i) {
    i_info(item_pars[i, ],
           theta = theta
    )
  })
  item <- data.frame(do.call(cbind, item))
  if (length(unique(theta)) == 1) {
    theta <- paste(theta, 1:length(theta), sep = "-")
  }
  rownames(item) <- theta
  colnames(item) <- rownames(item_pars)
  } else {
    if (ncol(item_pars) != 2 * K)
      stop("item_pars must have exactly 2*K columns.")

    n_items <- nrow(item_pars)
    n_theta <- length(theta)

    info_mat <- matrix(NA_real_, n_theta, n_items)

    for (i in seq_len(n_items)) {
      info_mat[, i] <- i_info(
        theta      = theta,
        item_pars = item_pars[i, , drop = FALSE],
        K         = K
      )
    }

    info_mat <- data.frame(info_mat)
    colnames(info_mat) <- rownames(item_pars)
    rownames(info_mat) <- theta

    item <- info_mat
  }
  item <- structure(
    item,
    class = c("iifs", class(item))
  )
  return(item)
}

#' Test Information Function (TIF)
#'
#' Compute the test information function of a test given a matrix of item information functions computed with the \code{item_info()} function. See \code{Details}.
#'
#' @param iifs object of class \code{iifs} containing the item information functions
#' @param fun \code{character} defining the function for the computation of the TIF, either by summing the items (sum) or by computing the mean (mean)
#'
#' @returns A \code{data.frame} of class \code{tif} with two columns: (i) \code{theta} containing the latent trait values, and (ii) \code{tif} containing the TIF values computed as either the sum or the mean of the IIFs
#' @export
#'
#' @details
#' The test infromation function (TIF) for both polytomous and dichotomous items is computed as:
#'
#' \deqn{
#' \text{TIF}(\theta) = \sum_{i = 1}^{B} I_i(\theta)
#' }
#'
#' Where \eqn{B} is the item bank.
#'
#' @examples
#' # Set random seed for reproducibility
#' set.seed(123)
#'
#' # Generate latent trait values for 100 respondents
#' theta <- rnorm(100)
#'
#' # Define the number of items
#' n <- 5
#'
#' # Create item parameter matrix/data frame
#' # b = difficulty parameters
#' # a = discrimination parameters
#' # c = lower asymptote
#' # e = upper asymptote
#' item_par <- data.frame(
#'   b = runif(n, -3, 3),
#'   a = runif(n, 1.2, 1.9),
#'   c = rep(0, n),
#'   e = rep(1, n)
#' )
#'
#' # Compute item information functions (IIFs)
#' # using default theta values
#' iifs <- item_info(item_par)
#'
#' # Compute the test information function (TIF)
#' # by combining information across items
#' test_tif <- tif(iifs)
tif <- function(iifs, fun = "sum") {
  if (inherits(iifs, "iifs") == FALSE) {
    stop("I need an object of class iifs")
  }
  theta <- as.numeric(rownames(iifs))
  tif_fun <- switch(fun,
                    sum = rowSums,
                    mean = rowMeans)
  thetif <- data.frame(theta,
                       tif = tif_fun(iifs))
  thetif <- structure(
    thetif,
    class = c("tif", class(thetif)),
    source = fun
  )

  return(thetif)
}
