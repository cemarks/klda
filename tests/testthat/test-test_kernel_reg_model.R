# Test kernel regression model

vowel_train <- read.csv(
  system.file("extdata", "vowel_train.csv", package = "klda")
)
vowel_test <- read.csv(
  system.file("extdata", "vowel_test.csv", package = "klda")
)

vowel_train <- vowel_train[, which(names(vowel_train) != "row.names")]
vowel_test <- vowel_test[, which(names(vowel_test) != "row.names")]

## Preprocessing
predictor_vars <- paste("x", 1:10, sep = ".")
train_means <- apply(vowel_train[, predictor_vars], 2, mean)
train_stddevs <- apply(vowel_train[, predictor_vars], 2, sd)


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

## Verify linear model works

x <- vowel_train_norm[, grep("x.", names(vowel_train), fixed = TRUE)]
y <- vowel_train_norm[, "y"]
x_test <- vowel_test_norm[, grep("x.", names(vowel_test), fixed = TRUE)]
y_test <- vowel_test_norm[, "y"]

y_one_hot <- as.matrix(mltools::one_hot(
    data.table::as.data.table(as.factor(y))))

krm <- kernel_reg_model(
    x,
    y_one_hot,
    kernel_function = lineardot(),
    lambda = 1
)

train_predictions <- predict(krm)
test_predictions <- predict(krm, as.matrix(x_test))

test_that("Training predictions dimensions", {
  expect_equal(dim(train_predictions)[1], dim(x)[1])
  expect_equal(dim(train_predictions)[2], length(unique(y)))
})

test_that("Test predictions dimensions", {
  expect_equal(dim(test_predictions)[1], dim(x_test)[1])
  expect_equal(dim(test_predictions)[2], length(unique(y)))
})