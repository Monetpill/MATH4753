test_that("myncurve returns correct mean, sigma, and probability", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)
  expect_true(result$prob > 0 & result$prob < 1) # Ensure probability is valid
})
