context("Utility functions")

test_that("singleton", {
  expect_true( contains_singleton( c(1,2,1), 2 ))
  expect_false( contains_singleton( c(1,1), 2 ))
  expect_false( contains_singleton( c(1,2,2,1), 2 ))
})

test_that("count bits", {
  expect_equal( count_set_bits( c(1,2,3) ), 4)
  expect_equal( count_set_bits( c(2**16,0) ), 1)
  expect_equal( count_set_bits( c(2**16-1) ), 16)
  expect_equal( count_set_bits( c(1,2,2,3) ), 5)
})

test_that("mask to int and back", {

  m <- c(1,2,3)
  mi <- masks_to_int(m, 2)
  mim <- int_to_masks(mi, 2, 3)

  expect_equal( mi, 1 + 4*2 + 16*3 )
  expect_equal( m, mim )
  expect_error( masks_to_int(m, 1000) )

})

test_that("convert masks to target", {
  m <- c(1,2,3)
  M <- outer(c(1,2,3), c(1,2,3), bitwAnd)

  expect_equal( masks_to_target(m), M[upper.tri(M)] )
})
