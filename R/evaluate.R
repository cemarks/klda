#' Test model performance
#'
#' Compute the correct classification rate and misclassification rate
#' for a fit fda model.
#'
#' @param model_obj \code{mda} \code{fda} model.
#' @param newdata data frame providing data to investigate.  Must include
#' all of the predictor variables and the response variable.  If omitted,
#' training data will be used.
#' @param print_results logical.  If TRUE, results will be outputted to
#' stdout
#'
#' @return numeric correct classification rate.
#'
#' @export
#'
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
#' model <- kernel_fda(
#'   Species ~ .,
#'   train_data_norm,
#'   kerneldot = kernlab::rbfdot,
#'   lambda = 1,
#'   sigma = 0.1
#' )
#'
#' test_kda(model)
#' test_kda(model,test_data_norm)
test_kda <- function(
    model_obj,
    newdata,
    print_results = TRUE
) {
    if (missing(newdata)) {
        conf_matrix <- model_obj$confusion
        correct <- sum(diag(conf_matrix))
        total <- sum(conf_matrix)
    } else {
        y_name <- as.character(model_obj$terms)[2]
        predictions <- predict(model_obj, newdata)
        y <- newdata[, y_name]
        correct <- sum(
            as.character(predictions) == as.character(y)
        )
        total <- length(predictions)
    }
    misclass <- total - correct
    if (print_results) {
        cat("\n")
        cat(sprintf("Total Classifications: %i\n", total))
        cat(sprintf("Correct: %i\n", correct))
        cat(sprintf("Misclass: %i\n", misclass))
        cat(sprintf("Classification rate: %1.2f\n", correct / total))
    }

    return(correct / total)
}