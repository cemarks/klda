# Kernel Linear Discriminant Analysis in R


This repository contains a minimal code implementations of kernel Linear Discriminant Analysis (LDA) in R. For more information on LDA see pp. 106--119 and pp 440--455 of Hastie, T., Tibshirani, R., & Friedman, J. H. (2009), *The elements of statistical learning: data mining, inference, and prediction,* 2nd ed. New York: Springer, pp 106--119 and pp 440--455, available [online](https://hastie.su.domains/ElemStatLearn/printings/ESLII_print12_toc.pdf).  This implementation is a special case of the "Flexible Discriminant Analysis" developed on pp. 440--445 in Hastie, et al.


## Installation

This package can be installed from the `devtools` package:

```r
devtools::install_github("cemarks/klda")
```

Alternatively, clone the repository and build/install the package.

```bash
git clone https://github.com/cemarks/klda
R CMD INSTALL klda
```

## Usage

See the package documentation examples and vignettes.  Below is an abbreviated example from the package documentation.

```r
> data(iris)
#'
# Separate into training & test
data_permutation_order <- sample(nrow(iris))
train_cutoff <- round(0.75 * nrow(iris))
train_indices <- data_permutation_order[1:train_cutoff]
test_indices <- data_permutation_order[(train_cutoff + 1):nrow(iris)]
train_data_unnorm <- iris[train_indices, ]
test_data_unnorm <- iris[test_indices, ]

# Normalize data
means <- apply(train_data_unnorm[,1:4],2,mean)
stddevs <- apply(train_data_unnorm[,1:4],2,sd)
train_data_norm <- train_data_unnorm
test_data_norm <- test_data_unnorm

for (col in 1:4) {
  train_data_norm[, col] <- (train_data_unnorm[, col] -
    means[col]) / stddevs[col]
  test_data_norm[, col] <- (test_data_unnorm[, col] -
    means[col]) / stddevs[col]
}

# Fit model
model <- kernel_fda(
  Species ~ .,
  train_data_norm,
  kerneldot = kernlab::rbfdot,
  lambda = 1,
  sigma = 0.1
)

# Evaluate Performance
tng_performance <- test_kda(model)
test_performance <- test_kda(model,test_data_norm)

# Plot
plot_kda(model, test_data_norm)
```

**Enjoy!!!**
