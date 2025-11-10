
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/ylzzzzz/linearregression/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ylzzzzz/linearregression/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ylzzzzz/linearregression/graph/badge.svg)](https://app.codecov.io/gh/ylzzzzz/linearregression)
<!-- badges: end -->

## Overview

**`linearregression`** is a simplified version of R’s built-in `lm()`
function.  
It provides an implementation of ordinary least squares (OLS) using
basic matrix operations.

- **`toy_lm()`** takes in a design matrix `X` (predictors) and a
  response vector `y`,  
  fits a linear model using the ordinary least squares method,  
  and returns the estimated coefficients, fitted values, and residuals.

## Installation

You can install the development version of linearregression from GitHub
with:

``` r
# install.packages("pak")
pak::pak("ylzzzzz/linearregression")
```

## Usage:

``` r
library(linearregression)
## code
```

## Getting help

If you encounter a bug or have suggestions for improvement, please open
an issue on  
[GitHub](https://github.com/ylzzzzz/linearregression/issues).
