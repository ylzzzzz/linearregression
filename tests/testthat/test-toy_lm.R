test_that("toy_lm produces correct coefficients", {
  fit_toy <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  fit_lm <- lm(mpg ~ wt + hp + disp, data = mtcars)

  expect_equal(
    unname(fit_toy$coefficients),
    unname(coef(fit_lm)),
    tolerance = 1e-6
  )
})

test_that("toy_lm fitted values match lm", {
  fit_toy <- toy_lm(mpg ~ wt + hp + disp, data = mtcars)
  fit_lm <- lm(mpg ~ wt + hp + disp, data = mtcars)

  expect_equal(
    fit_toy$fitted,
    unname(fitted(fit_lm)),
    tolerance = 1e-6
  )
})

test_that("toy_lm errors for non-numeric response", {
  fake_data <- mtcars
  fake_data$mpg <- as.character(fake_data$mpg)

  expect_error(
    toy_lm(mpg ~ wt + hp + disp, data = fake_data),
    "numeric"
  )
})


