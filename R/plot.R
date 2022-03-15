#' Plot a flexible linear discriminant model
#'
#' Plots centroids and cannonical variates in specified coordinates.
#' The coordinate values must be strictly less than the number of classes.
#' (See p. 445, Hastie, et al.)
#' in the model.  The first coordinate correspondes to the largest eigenvalue.
#'
#' @param model_obj \code{mda} \code{fda} object.
#' @param newdata data frame new data to be plotted.  Must have same names as
#' data used for fitting.  If omitted, training data will be plotted.
#' @param xcoord integer coordinate to use on x axis.
#' @param ycoord integer coordinate to use on y axis.
#'
#' @return \code{ggplot2} \code{ggplot} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{kernel_fda}},
#' \code{\link{linear_fda}}
#'
#' @export
#' @examples
#' data(iris)
#'
#' # Normalize data
#' means <- apply(iris[,1:4], 2, mean)
#' stddevs <- apply(iris[,1:4], 2, sd)
#' iris_normalized <- iris
#' for (col in 1:4) {
#'   iris_normalized[, col] <- (iris[, col] - means[col]) /
#'     stddevs[col]
#' }
#'
#' model <- kernel_fda(
#'   Species ~ .,
#'   iris_normalized,
#'   kerneldot = kernlab::rbfdot,
#'   lambda = 1,
#'   sigma = 0.1
#' )
#'
#' plot(model)
plot_kda <- function(model_obj, newdata, xcoord = 1, ycoord=2) {
    if (missing(newdata)) {
        cannonical_variates <- predict(model_obj, type = "variates")
        classes <- predict(model_obj)
    } else{
        cannonical_variates <- predict(model_obj, newdata, type = "variates")
        classes <- predict(model_obj, newdata)
    }
    centroid_df <- data.frame(
        x = model_obj$means[, xcoord],
        y = model_obj$means[, ycoord],
        class = row.names(model_obj$means),
        type = rep("centroid", nrow(model_obj$means))
    )
    variate_df <- data.frame(
        x = cannonical_variates[, xcoord],
        y = cannonical_variates[, ycoord],
        class = as.character(classes),
        type = rep("variate", nrow(cannonical_variates))
    )
    df <- rbind(centroid_df, variate_df)
    g <- ggplot2::ggplot(
        data = df,
        mapping = ggplot2::aes(x = x, y = y, color = class,
          shape = type, size = type)
    ) +
      ggplot2::geom_point(stroke = 3) +
      ggplot2::scale_size_manual(
          values = c(variate = 0.5, centroid = 8),
          limits = c("variate", "centroid"),
          breaks = c("variate", "centroid"),
          guide = "none"
      ) +
      ggplot2::scale_shape_manual(
          values = c(variate = 20, centroid = 1),
          limits = c("variate", "centroid"),
          breaks = c("variate", "centroid"),
          guide = "none"
      ) +
      ggplot2::guides(color = "none") +
      ggplot2::xlab(paste("Coordinate", xcoord, sep = " ")) +
      ggplot2::ylab(paste("Coordinate", ycoord, sep = " "))

    return(g)
}