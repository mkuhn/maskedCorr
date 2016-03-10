context("Utility functions")

test_that("singletons", {
  expect_true( contains_singletons( c(1,2,1), 2 ))
  expect_false( contains_singletons( c(1,1), 2 ))
  expect_false( contains_singletons( c(1,2,2,1), 2 ))
})
