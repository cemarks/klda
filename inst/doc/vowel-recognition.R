## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(klda)

## -----------------------------------------------------------------------------
vowel_train <- read.csv(
  system.file("extdata", "vowel_train.csv", package = "klda")
)
vowel_test <- read.csv(
  system.file("extdata", "vowel_test.csv", package = "klda")
)

## -----------------------------------------------------------------------------
# Remove row.names
vowel_train <- vowel_train[, which(names(vowel_train) != "row.names")]
vowel_test <- vowel_test[, which(names(vowel_test) != "row.names")]

## Get predictor means and standard deviations from the training data.
predictor_vars <- paste("x", 1:10, sep = ".")
train_means <- apply(vowel_train[, predictor_vars], 2, mean)
train_stddevs <- apply(vowel_train[, predictor_vars], 2, sd)

## A function to help normalize columns
normalize_x <- function(
    x,
    means,
    stddevs,
    predictor_vars
) {
    x_norm <- x
    for (pv in predictor_vars) {
        x_norm[, pv] <- (x[, pv] - means[pv]) / stddevs[pv]
    }
    return(x_norm)
}

## Produce normalized training and test data
vowel_train_norm <- normalize_x(
    vowel_train,
    train_means,
    train_stddevs,
    predictor_vars
)

vowel_test_norm <- normalize_x(
    vowel_test,
    train_means,
    train_stddevs,
    predictor_vars
)

## -----------------------------------------------------------------------------
start_time <- Sys.time()
lda <- linear_fda(
    y~.,
    vowel_train_norm
)
end_time <- Sys.time()

cat(sprintf("Model time: %1.2f\n", end_time - start_time))

g <- plot_kda(lda)
print(g)

## -----------------------------------------------------------------------------
g <- plot_kda(lda, vowel_test_norm, 1, 3)
print(g)

## -----------------------------------------------------------------------------
# Training performance
training_performance <- test_kda(lda)

# Test performance
test_performance <- test_kda(lda, vowel_test_norm)

## -----------------------------------------------------------------------------
start_time <- Sys.time()
klda <- kernel_fda(
    y~.,
    vowel_train_norm
)
end_time <- Sys.time()

cat(sprintf("Model time: %1.2f\n", end_time - start_time))

g <- plot_kda(klda)
print(g)

g <- plot_kda(klda, vowel_test_norm, 1, 3)
print(g)

# Training performance
training_performance <- test_kda(klda)

# Test performance
test_performance <- test_kda(klda, vowel_test_norm)

## -----------------------------------------------------------------------------
start_time <- Sys.time()
kda <- kernel_fda(
    y~.,
    vowel_train_norm,
    kerneldot = kernlab::rbfdot,
    lambda = 1,
    sigma = 0.1
)
end_time <- Sys.time()

cat(sprintf("Model time: %1.2f\n", end_time - start_time))

g <- plot_kda(kda)
print(g)

g <- plot_kda(lda, vowel_test_norm, 1, 3)
print(g)

## -----------------------------------------------------------------------------
# Training performance
training_performance <- test_kda(lda)

# Test performance
test_performance <- test_kda(lda, vowel_test_norm)

## -----------------------------------------------------------------------------
test_predictions <- predict(kda, vowel_test_norm)
for (pt in 1:10) {
  cat(sprintf("Point %i: Actual %i, Predicted %i\n",
    pt, test_predictions[pt], vowel_test_norm$y[pt]))
}

