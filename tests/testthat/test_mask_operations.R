context("Mask operations")

test_that("increment mask", {
  expect_equal( increment_mask( c(0,0,0), 4), c(1,0,0) )
  expect_equal( increment_mask( c(1,1,1), 4), c(2,1,1) )
  expect_equal( increment_mask( c(3,1,1), 4), c(0,2,1) )
  expect_equal( increment_mask( c(3,3,3), 4), NA )
})
