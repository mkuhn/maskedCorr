context("Utility functions")

test_that("singleton", {
  expect_true( contains_singleton( c(1,2,1), 2 ))
  expect_false( contains_singleton( c(1,1), 2 ))
  expect_false( contains_singleton( c(1,2,2,1), 2 ))
})

test_that("count bits", {
  expect_equal( count_set_bits( c(1,2,3), 2 ), 4)
  expect_equal( count_set_bits( c(1,0), 2 ), 1)
  expect_equal( count_set_bits( c(1,2,2,3), 2 ), 5)
})
