test_that("toy_lm produces correct coefficients", {
  data(mtcars)

  # Fit both models
  fit_toy <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  fit_lm  <- lm(mpg ~ wt + hp + disp, data = mtcars)

  # Compare coefficients
  expect_equal(round(fit_toy$coefficients, 6),
               round(coef(fit_lm), 6))
})

test_that("toy_lm detects collinearity", {
  data(mtcars)
  # Create a collinear predictor
  mtcars$dup_wt <- mtcars$wt
  expect_error(
    toy_lm(mpg ~ wt + dup_wt + hp, data = mtcars),
    regexp = "singular|collinear"
  )
})

test_that("toy_lm returns residuals and fitted values of correct length", {
  data(mtcars)
  fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  expect_length(fit$fitted, nrow(mtcars))
  expect_length(fit$residuals, nrow(mtcars))
})

