#' Flexible Discriminant Analysis with user-defined kernel
#'
#' Builds a regularized flexible discriminant model from user supplied
#' data, kernel function, and regularization parameter.
#'
#' @param formula formula of the form ‘y~x’ it describes the response and the
#' predictors.  The formula can be more complicated, such as
#' ‘y~log(x)+z’ etc (see ‘formula’ for more details).  The
#' response should be a factor representing the response
#' variable, or any vector that can be coerced to such (such as
#' a logical variable).
#' @param data data.frame containing predictor and response data.  The predictor
#' data should be centered and scaled (normalized).
#' @param kerneldot method from \code{\link[kernlab]{dots}} methods or
#' user-defined kernel function with the same signature.  Default is
#' linear kernel.
#' @param lambda numeric regularization coefficient.  Default is 1.
#' @param ... additional parameters passed to kernel.dot
#'
#' @return \code{mda} \code{fda} object.
#'
#' @references Hastie, T., Tibshirani, R., & Friedman, J. H. (2009),
#' *The elements of statistical learning: data mining, inference,
#' and prediction,* 2nd ed. New York: Springer, pp 106--119 and pp 440--455.
#' \url{https://hastie.su.domains/ElemStatLearn/printings/ESLII_print12_toc.pdf}
#'
#' @seealso \code{\link[mda]{fda}}, \code{\link[kernlab]{dots}},
#' \code{\link{linear_fda}},
#' \url{https://hastie.su.domains/ElemStatLearn/printings/ESLII_print12_toc.pdf}
#' @export
#'
#' @examples
#' data(iris)
#'
#' # Normalize data
#' means <- apply(iris[,1:4],2,mean)
#' stddevs <- apply(iris[,1:4],2,sd)
#' iris_normalized <- iris
#' for (col in 1:4) {
#'   iris_normalized[,col] <- (iris[,col] - means[col])/stddevs[col]
#' }
#'
#' model <- kernel_fda(
#'   Species ~ .,
#'   iris_normalized,
#'   kerneldot = kernlab::rbfdot,
#'   lambda = 1,
#'   sigma = 0.1
#' )
kernel_fda <- function(
    formula,
    data,
    kerneldot = NULL,
    lambda = 1,
    ...
) {
    if (is.null(kerneldot)) {
        kerneldot <- lineardot
    }
    model <- mda::fda(
        formula = formula,
        data = data,
        method = kernel_reg_model,
        kernel_function = kerneldot(...),
        lambda = lambda
    )
    return(model)
}

#' Flexible Discriminant Analysis with linear regression model
#'
#' Builds a regularized flexible discriminant model from user supplied
#' data and L2-regularization parameter, using linear regression model.
#' This method is useful for quickly comparing more complicated models
#' to a linear model and for debugging more complicated methods.  By
#' itself, it does not provide functionality beyond what is already
#' provided in the \code{mda} package, and might be less efficient
#' than equivalent methods there.
#'
#' @param formula formula of the form ‘y~x’ it describes the response and the
#' predictors.  The formula can be more complicated, such as
#' ‘y~log(x)+z’ etc (see ‘formula’ for more details).  The
#' response should be a factor representing the response
#' variable, or any vector that can be coerced to such (such as
#' a logical variable).
#' @param data data.frame containing predictor and response data.  The
#' predictor data should be centered and scaled (normalized).
#' @param lambda numeric regularization coefficient.  Default is 1.
#'
#' @return \code{mda} \code{fda} object.
#'
#' @seealso \code{\link[mda]{fda}}, \code{\link[kernlab]{dots}},
#' \code{\link{kernel_fda}},
#' \url{https://hastie.su.domains/ElemStatLearn/printings/ESLII_print12_toc.pdf}
#' @export
#'
#' @examples
#' data(iris)
#'
#' # Normalize data
#' means <- apply(iris[,1:4],2,mean)
#' stddevs <- apply(iris[,1:4],2,sd)
#' iris_normalized <- iris
#' for (col in 1:4) {
#'   iris_normalized[,col] <- (iris[,col] - means[col])/stddevs[col]
#' }
#'
#' model <- linear_fda(
#'   Species ~ .,
#'   iris_normalized,
#'   lambda = 1
#' )
linear_fda <- function(
    formula,
    data,
    lambda = 1
) {
    model <- mda::fda(
        formula = formula,
        data = data,
        method = linear_reg_model,
        lambda = lambda
    )
    return(model)
}

#' Linear kernel method
#'
#' Generate linear kernel matrix (i.e., dot product).  This
#' function is meant to be used to build a kernel_model object,
#' (from the `kernel_fda` method) and not meant to be used explicity.
#'
#' @param add_ones logical if TRUE will add an intercept column
#' (i.e. a column of 1's) to input matricies
#'
#' @return function that takes to matrices and returns the
#' dot product
#'
#' @seealso \code{\link{kernel_fda}}, \code{\link[kernlab]{dots}}
lineardot <- function(add_ones = FALSE) {
    addones <- add_ones
    linear_kernel <- function(x1, x2) {
        x1_matrix <- x1
        x2_matrix <- x2
        if (addones) {
            x1_matrix <- append(1, x1_matrix)
            x2_matrix <- append(1, x2_matrix)
        }
        return(x1_matrix %*% x2_matrix)
    }

    return(linear_kernel)
}