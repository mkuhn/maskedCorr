library(maskedCorr)
context("Masked correlations")

set.seed(42)
v1 <- rnorm(10)
v2 <- rnorm(10)

test_that("successful masked correlation", {
  expect_equal( masked_corr(v1, v2, 3, list(c(1,5), c(6,10))), cor(v1, v2) )
  expect_equal( masked_corr(v1, v2, 1, list(c(1,5), c(6,10))), cor(v1[1:5], v2[1:5]) )
  expect_equal( masked_corr(v1, v2, 2, list(c(1,5), c(6,10))), cor(v1[6:10], v2[6:10]) )
  expect_equal( masked_corr(c(v1[1:9], NA), v2, 2, list(c(1,5), c(6,10))), as.numeric(NA) )
  expect_equal( masked_corr(c(v1[1:9], NA), v2, 1, list(c(1,5), c(6,10))), cor(v1[1:5], v2[1:5]) )
})

test_that("unsuccessful masked correlation", {
  expect_error( masked_corr(v1, v2, 3, list("A", "B")) )
})

test_that("successful computing all masked correlations", {
  expect_equal( all_pairwise_correlations(v1, v2, 3, 3, list(c(1,5), c(6,10))),
                c(cor(v1[1:5], v2[1:5]), cor(v1[6:10], v2[6:10]), cor(v1, v2)) )

  expect_equal( all_pairwise_correlations(v1, v2, 1, 3, list(c(1,5), c(6,10))),
                c(cor(v1[1:5], v2[1:5]), NA, NA) )
})

