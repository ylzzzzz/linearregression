
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Fit a Linear Regression Model (Simplified lm)
#'
#' A simplified implementation of linear regression using the normal equations.
#' Takes a formula and data frame, constructs the design matrix, and returns
#' coefficients, fitted values, residuals, and useful model statistics.
#'
#' @importFrom stats model.frame model.matrix model.response
#'
#' @param formula A model formula such as \code{mpg ~ wt + hp}.
#' @param data A data frame containing the variables in the formula.
#'
#' @return A list containing:
#' \item{coefficients}{Estimated regression coefficients}
#' \item{fitted}{Vector of fitted values}
#' \item{residuals}{Vector of residuals}
#' \item{r.squared}{R-squared}
#' \item{adj.r.squared}{Adjusted R-squared}
#' \item{mse}{Mean squared error}
#' \item{residual.se}{Residual standard error}
#' \item{X}{Design matrix (stored for summary)}
#' \item{y}{Response vector (stored for summary)}
#'
#' @examples
#' # Fit toy_lm to the mtcars dataset
#' fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
#' fit$coefficients
#'
#' @export
toy_lm <- function(formula, data) {

  ## -------------------------
  ## Build model frame
  ## -------------------------
  mf <- model.frame(formula, data)
  X <- model.matrix(formula, mf)
  y <- model.response(mf)

  ## -------------------------
  ## Check data validity
  ## -------------------------
  if (!is.numeric(y)) {
    stop("Response variable must be numeric.")
  }

  if (!is.matrix(X)) {
    stop("Predictors must form a numeric matrix.")
  }

  ## -------------------------
  ## Check X'X invertibility
  ## -------------------------
  XtX <- t(X) %*% X

  if (det(XtX) <= .Machine$double.eps) {
    stop("Matrix X'X is nearly singular; predictors may be collinear.")
  }

  ## -------------------------
  ## Fit model using normal equations
  ## -------------------------
  beta_hat <- solve(XtX) %*% t(X) %*% y
  fitted <- X %*% beta_hat
  residuals <- y - fitted

  ## -------------------------
  ## Compute diagnostics
  ## -------------------------
  n <- length(y)
  p <- ncol(X)

  sse <- sum(residuals^2)
  sst <- sum((y - mean(y))^2)

  mse <- sse / (n - p)
  residual_se <- sqrt(mse)

  r_squared <- 1 - sse / sst
  adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p)

  ## -------------------------
  ## Prepare coefficient names
  ## -------------------------
  beta_hat <- drop(beta_hat)
  names(beta_hat) <- colnames(X)

  ## -------------------------
  ## Return model object
  ## -------------------------

  res <- list(
    coefficients = beta_hat,
    fitted = as.vector(fitted),
    residuals = as.vector(residuals),
    X = X,
    y = y
  )
  res$call <- match.call()
  class(res) <- "toy_lm"
  return(res)

}

