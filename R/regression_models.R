#' Kernel Regression
#'
#' Construct a linear regression model using the user-supplied
#' kernel function.  This function is called by \code{kernel_fda} and
#' is not intended to be used outside of that context.
#'
#' @param x matrix of predictor data.
#' @param y vector or matrix of response data.
#' @param w numeric not used.  Intended to be weights to be applied to training
#' data.
#' @param kernel_function function to compute the kernel.
#' @param lambda numeric regularization coefficient.  Default is 1.
#' @return function that takes to matrices and returns the
#' dot product
#' @seealso \code{\link{kernel_fda}}
#' @export
kernel_reg_model <- function(
    x,
    y,
    w,
    kernel_function,
    lambda = 1
) {
    if (is.null(kernel_function)) {
        kernel_function <- lineardot()
    }
    kernel_matrix <- kernlab::kernelMatrix(kernel = kernel_function,
        x = as.matrix(x))
    alpha <- solve(kernel_matrix + diag(lambda, nrow = nrow(kernel_matrix)), y)
    y_hat <- kernel_matrix %*% alpha
    model <- structure(
        list(
            x = x,
            y = y,
            alpha = alpha,
            lambda = lambda,
            kernel_fn = kernel_function,
            fitted.values = y_hat
        ),
        class = "kernel_regression")
    return(model)
}


#' Linear Regression
#'
#' Construct a L2-regularized linear regression model.
#' This function is called by \code{kernel_fda} and
#' is not intended to be used outside of that context.
#'
#' @param x matrix of predictor data.
#' @param y vector or matrix of response data.
#' @param w numeric not used.  Intended to be weights to be applied to training
#' data.
#' @param lambda numeric regularization coefficient.  Default is 1.
#' @return function that takes to matrices and returns the
#' dot product
#' @seealso \code{\link{linear_fda}}
#' @export
linear_reg_model <- function(
    x,
    y,
    w,
    lambda = 1
) {
    b <- solve(
        t(x) %*% x + diag(lambda, nrow = ncol(x)),
        t(x) %*% y
    )
    y_hat <- x %*% b
    model <- structure(
        list(
            x = x,
            y = y,
            beta = b,
            lambda = lambda,
            fitted.values = y_hat
        ),
        class = "linear_regression"
    )
    return(model)
}


