
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Fit a Linear Regression Model (Simplified lm)
#'
#' A simplified linear regression implementation using the normal equations.
#'
#' Fits a multiple linear regression model using ordinary least squares,
#' assuming all predictor variables are numeric. The function computes
#' coefficient estimates, standard errors, t-tests, fitted values,
#' residuals, model diagnostics, and an overall F-test.
#'
#' @importFrom stats model.frame model.matrix model.response pt
#' @importFrom stats model.frame model.matrix model.response pt pf
#'
#' @param formula A model formula (e.g., mpg ~ wt + hp)
#' @param data A data frame containing the variables in the formula
#'
#' @return A list containing:
#' \item{call}{The matched function call}
#' \item{coefficients}{Coefficient table with SE, t, p}
#' \item{fitted}{Fitted values}
#' \item{residuals}{Residuals}
#'
#' \item{residual.se}{Residual standard error}
#' \item{mse}{Mean squared error}
#' \item{r.squared}{R-squared}
#' \item{adj.r.squared}{Adjusted R-squared}
#'
#' \item{f.statistic}{F-statistic testing overall model significance}
#' \item{f.df}{Numerator and denominator degrees of freedom for the F-test}
#' \item{f.pvalue}{P-value for the F-test}
#'
#' \item{X}{Model matrix}
#' \item{y}{Response vector}

#'
#' @examples
#' fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
#' fit$coefficients
#'
#' @export
toy_lm <- function(formula, data) {

  # model frame
  mf <- model.frame(formula, data)
  X <- model.matrix(formula, mf)
  y <- model.response(mf)

  # inputs data type
  pred_vars <- all.vars(formula)[-1]
  if (!all(sapply(data[pred_vars], is.numeric))) {
    stop("Predictor variables must be numeric.")
  }


  XtX <- t(X) %*% X
  if (det(XtX) <= .Machine$double.eps)
    stop("Matrix X'X is singular.")

  # compute beta hat
  beta_hat <- solve(XtX) %*% t(X) %*% y
  fitted <- X %*% beta_hat
  residuals <- y - fitted

  # statistics
  n <- length(y)
  p <- ncol(X)
  sse <- sum(residuals^2)
  mse <- sse / (n - p)
  residual_se <- sqrt(mse)
  sst <- sum((y - mean(y))^2)
  r2 <- 1 - sse/sst
  adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - p)

  # coefficient table
  XtX_inv <- solve(XtX)
  se <- sqrt(diag(mse * XtX_inv))
  beta_hat <- drop(beta_hat)
  names(beta_hat) <- colnames(X)

  # t-test
  tvals <- beta_hat / se
  pvals <- 2 * pt(abs(tvals), df = n - p, lower.tail = FALSE)

  coef_table <- cbind(
    Estimate = beta_hat,
    Std.Error = se,
    t.value = tvals,
    p.value = pvals
  )

  # F-test
  msr <- (sst - sse) / (p - 1)
  fstat <- msr / mse          # F statistic
  df1 <- p - 1    # numerator df
  df2 <- n - p    # denominator df
  f_pvalue <- pf(fstat, df1, df2, lower.tail = FALSE)

  # output
  res <- list(
    call = match.call(),
    coefficients = coef_table,
    fitted = as.vector(fitted),
    residuals = as.vector(residuals),

    residual.se = residual_se,
    mse = mse,
    r.squared = r2,
    adj.r.squared = adj_r2,

    f.statistic = fstat,
    f.df = c(df1, df2),
    f.pvalue = f_pvalue,

    X = X,
    y = y
  )

  class(res) <- "toy_lm"
  return(res)
}


