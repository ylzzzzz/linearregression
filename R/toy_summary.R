#' Summary Method for toy_lm Models
#'
#' Provides coefficient table (estimate, standard error, t-statistic, p-value),
#' along with residual standard error, R-squared, and adjusted R-squared.
#'
#' @param object An object returned by \code{toy_lm()}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the model object after printing the summary.
#'
#' @export
summary.toy_lm <- function(object, ...) {

  # Extract components
  coeff <- object$coefficients
  X <- object$X
  y <- object$y
  residuals <- object$residuals
  fitted <- object$fitted

  n <- length(y)
  p <- length(coeff)

  # Residual standard error
  sse <- sum(residuals^2)
  mse <- sse / (n - p)
  residual_se <- sqrt(mse)

  # Standard errors
  XtX_inv <- solve(t(X) %*% X)
  se <- sqrt(diag(mse * XtX_inv))

  # t-values and p-values
  tvals <- coeff / se
  pvals <- 2 * pt(abs(tvals), df = n - p, lower.tail = FALSE)

  # R-squared
  sst <- sum((y - mean(y))^2)
  r2 <- 1 - sse/sst
  adj_r2 <- 1 - (1 - r2)*(n - 1)/(n - p)

  # Build coefficient table
  coef_table <- cbind(
    Estimate = coeff,
    Std.Error = se,
    t.value = tvals,
    p.value = pvals
  )
  rownames(coef_table) <- names(coeff)

  # Print output (formatted)
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  print(round(coef_table, 4))

  cat("\nResidual standard error:", round(residual_se, 4),
      "on", n - p, "degrees of freedom\n")
  cat("Multiple R-squared:", round(r2, 4),
      ", Adjusted R-squared:", round(adj_r2, 4), "\n")

  # Return invisible list (to avoid auto-printing)
  invisible(list(
    coefficients = coef_table,
    r.squared = r2,
    adj.r.squared = adj_r2,
    residual.se = residual_se
  ))
}

