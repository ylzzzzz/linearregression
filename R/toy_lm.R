
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Fit a simple linear regression model
#'
#' @param formula A formula specifying the model (e.g., mpg ~ wt + hp + disp)
#' @param data A data frame containing the variables in the model
#' @return A list containing coefficients, fitted values, and residuals
#' @examples
#' # Fit a simple model using mtcars dataset
#' fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
#' fit
#'
#' @export
toy_lm <- function(formula, data) {
  mf <- model.frame(formula, data)
  X <- model.matrix(formula, mf)
  y <- model.response(mf)

  # Check numeric and invertibility
  if (!is.numeric(y) || !is.matrix(X))
    stop("Response must be numeric and predictors must form a numeric matrix.")

  XtX <- t(X) %*% X
  if (det(XtX) == 0)
    stop("Matrix X'X is singular; predictors may be collinear.")

  beta_hat <- solve(XtX) %*% t(X) %*% y
  fitted <- X %*% beta_hat
  residuals <- y - fitted

  list(
    coefficients = as.vector(beta_hat),
    fitted = as.vector(fitted),
    residuals = as.vector(residuals)
  )
}
