
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Fit a simple linear regression model
#'
#' @param X Numeric matrix or data frame of predictors
#' @param y Numeric response vector
#' @return A list containing coefficients, fitted values, and residuals
#' @examples
#' X <- cbind(1, 1:4)
#' y <- c(2, 4, 5, 7)
#' toy_lm(X, y)
#' @export
toy_lm <- function(X, y) {
  # Check dimensions
  if (nrow(X) != length(y))
    stop("Number of rows in X must match length of y.")

  # Check for full rank
  if (qr(X)$rank < ncol(X))
    stop("Design matrix X is not full rank; (X'X) is not invertible.")

  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  fitted <- X %*% beta
  residuals <- y - fitted
  list(
    coefficients = drop(beta),
    fitted = drop(fitted),
    residuals = drop(residuals)
  )
}

