context("Mask initialization")

test_that("successful mask detection", {
  expect_equal( detect_mask(1:10, list(c(1,5), c(6,10))), 1+2 )
  expect_equal( detect_mask(1:10, list(c(1,5), c(6,6), c(7,10))), 1+2+4 )
  expect_equal( detect_mask(c(NA,NA,3:10), list(c(1,2), c(3,6), c(7,10))), 2+4 )
})

test_that("detect mismatched masks", {
  expect_equal( detect_mask(c(NA,2:10), list(c(1,2), c(3,6), c(7,10))), NA )
})
