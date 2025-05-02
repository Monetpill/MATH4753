test_that("my squared function works", {
  l <- mysq(2:4)
  expect_equal(l[1], 4)
})
