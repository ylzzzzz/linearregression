
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/ylzzzzz/linearregression/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ylzzzzz/linearregression/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ylzzzzz/linearregression/graph/badge.svg)](https://app.codecov.io/gh/ylzzzzz/linearregression)
<!-- badges: end -->

## Overview

`linearregression` provides a toy implementation of ordinary least
squares (OLS) using the normal equations.

- `toy_lm()` accepts a model formula and a data frame, checks that all
  predictors are numeric, fits the OLS model using `β̂ = (XᵀX)⁻¹ Xᵀy`and
  returns a full coefficient table (estimates, standard errors,
  t‐statistics, and p‐values).

- Additional statistics returned include fitted values, residuals, mean
  squared error (MSE), residual standard error (RSE), R², adjusted R²,
  and the overall F‐test (statistic, degrees of freedom, and p‐value).

## Installation

You can install the development version of linearregression from GitHub
with:

``` r
remotes::install_github(
  "ylzzzzz/linearregression",
  build_vignettes = TRUE,
  dependencies = TRUE
)
```

## Usage:

``` r
library(linearregression)

# Fit a model
fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)

# View the coefficient table
fit$coefficients
#>                  Estimate  Std.Error     t.value      p.value
#> (Intercept) 37.1055052690 2.11081525 17.57875558 1.161936e-16
#> wt          -3.8008905826 1.06619064 -3.56492586 1.330991e-03
#> hp          -0.0311565508 0.01143579 -2.72447633 1.097103e-02
#> disp        -0.0009370091 0.01034974 -0.09053451 9.285070e-01

# Additional model statistics
fit$r.squared        # R-squared
#> [1] 0.8268361
fit$adj.r.squared    # Adjusted R-squared
#> [1] 0.8082829
fit$f.statistic      # F-statistic for overall significance
#> [1] 44.56552
fit$f.pvalue         # p-value for the F-test
#> [1] 8.649588e-11
```

More detailed tutorials can be found in the vignettes.

## Getting help

If you encounter a bug or have suggestions for improvement, please open
an issue on  
[GitHub](https://github.com/ylzzzzz/linearregression/issues).
