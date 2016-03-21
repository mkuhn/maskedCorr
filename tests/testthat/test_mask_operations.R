context("Mask operations")

test_that("increment mask", {
  expect_equal( increment_mask( c(0,0,0), 2, F, F), c(1,0,0) )
  expect_equal( increment_mask( c(0,0,0), 2, T, F), c(1,1,0) )
  expect_equal( increment_mask( c(1,1,1), 2, F, F), c(2,1,1) )
  expect_equal( increment_mask( c(3,1,1), 2, F, F), c(0,2,1) )
  expect_equal( increment_mask( c(3,3,0), 2, T, F), c(1,0,1) )
  expect_equal( increment_mask( c(2,3,3), 2, T, F), c(3,3,3) )
  expect_equal( increment_mask( c(3,3,3), 2, F, F), as.integer(NA) )

  expect_equal( increment_mask( c(3,1,1), 2, F, T), c(1,2,1) )
  expect_error( increment_mask( c(0,0,0), 2, F, T) )

})
