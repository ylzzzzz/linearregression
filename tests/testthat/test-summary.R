test_that("summary.toy_lm runs without error", {
  fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  expect_no_error(summary(fit))
})

test_that("summary output contains key fields", {
  fit <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  out <- capture.output(summary(fit))

  expect_true(any(grepl("Std. Error", out)))
  expect_true(any(grepl("t value", out)))
  expect_true(any(grepl("Pr\\(>\\|t\\|\\)", out)))
  expect_true(any(grepl("R-squared", out)))
})
