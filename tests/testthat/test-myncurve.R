test_that("myncurve returns correct mu", {
  result <- myncurve(mu=10, sigma=5, a=6)
  expect_equal(result$mu, 10)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(mu=10, sigma=5, a=6)
  expect_equal(result$sigma, 5)
})

test_that("myncurve returns correct area", {
  result <- myncurve(mu=10, sigma=5, a=6)
  expect_equal(result$area, round(pnorm(6, mean=10, sd=5), 4))
})
