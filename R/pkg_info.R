#' klda: Kernel Linear Discriminant Analysis
#'
#' This package extends the Flexible Discriminant Analysis (FDA) 
#' classification method presented in "The Elements of Statistical 
#' Learning and Data Mining, 2nd Ed," (Hastie, et al, 2017, 
#' see pp 106--119,440--455) to make use of kernel regression methods. 
#'
#' @author Christopher E. Marks, \email{cemarks@@alum.mit.edu}
#' @references \url{https://github.com/cemarks/linear-discriminant}
#' @seealso \code{\link{mda}}
#' @keywords mda
#'
#' @importFrom stats predict
#' @importFrom mda fda
#' @importFrom kernlab kernelMatrix
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_size_manual
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @docType package
#' @name klda
#' @examples
#' data(iris)
#'
#' # Separate into training & test
#' data_permutation_order <- sample(nrow(iris))
#' train_cutoff <- round(0.75 * nrow(iris))
#' train_indices <- data_permutation_order[1:train_cutoff]
#' test_indices <- data_permutation_order[(train_cutoff + 1):nrow(iris)]
#' train_data_unnorm <- iris[train_indices, ]
#' test_data_unnorm <- iris[test_indices, ]
#'
#' # Normalize data
#' means <- apply(train_data_unnorm[,1:4],2,mean)
#' stddevs <- apply(train_data_unnorm[,1:4],2,sd)
#' train_data_norm <- train_data_unnorm
#' test_data_norm <- test_data_unnorm
#'
#' for (col in 1:4) {
#'   train_data_norm[, col] <- (train_data_unnorm[, col] -
#'     means[col]) / stddevs[col]
#'   test_data_norm[, col] <- (test_data_unnorm[, col] -
#'     means[col]) / stddevs[col]
#' }
#'
#' # Fit model
#' model <- kernel_fda(
#'   Species ~ .,
#'   train_data_norm,
#'   kerneldot = kernlab::rbfdot,
#'   lambda = 1,
#'   sigma = 0.1
#' )
#'
#' # Evaluate Performance
#' tng_performance <- test_kda(model)
#' test_performance <- test_kda(model,test_data_norm)
#'
#' # Plot
#' plot_kda(model, test_data_norm)
NULL