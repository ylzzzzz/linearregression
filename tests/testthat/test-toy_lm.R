test_that("toy_lm runs and returns required components", {
  fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)

  expect_s3_class(fit, "toy_lm")
  expect_type(fit$coefficients, "double")
  expect_true(is.matrix(fit$coefficients))
  expect_true(all(c("Estimate", "Std.Error", "t.value", "p.value") %in% colnames(fit$coefficients)))

  expect_length(fit$fitted, nrow(mtcars))
  expect_length(fit$residuals, nrow(mtcars))

  expect_true(is.numeric(fit$r.squared))
  expect_true(is.numeric(fit$adj.r.squared))
  expect_true(is.numeric(fit$mse))
  expect_true(is.numeric(fit$residual.se))
})

test_that("toy_lm coefficients are close to lm() results", {
  fit_toy <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  fit_lm  <- lm(mpg ~ wt + hp + disp, data = mtcars)

  # Extract coefficient vector from toy table
  toy_coef <- fit_toy$coefficients[, "Estimate"]

  expect_equal(
    round(toy_coef, 5),
    round(coef(fit_lm), 5),
    tolerance = 1e-5
  )
})

test_that("toy_lm detects singular (collinear) design matrix", {
  dat <- mtcars
  dat$dup <- dat$wt  # make predictor perfectly collinear

  expect_error(
    toy_lm(mpg ~ wt + dup, data = dat),
    "singular"
  )
})

test_that("toy_lm errors when response y is not numeric", {
  dat <- mtcars
  dat$mpg <- as.character(dat$mpg)

  expect_error(
    toy_lm(mpg ~ wt + hp, data = dat),
    "numeric"
  )
})

test_that("toy_lm errors when X is not a matrix", {

  local_stub(model.matrix, function(formula, data) {
    return(as.data.frame(data))   # return NON-matrix
  })

  expect_error(
    toy_lm(mpg ~ wt + hp, data = mtcars),
    "Predictor matrix X is not numeric"
  )
})




