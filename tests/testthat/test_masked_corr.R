context("Masked correlations")

set.seed(42)
v1 <- rnorm(10)
v2 <- rnorm(10)
v3 <- rnorm(10)

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

test_that("successfully computing all pairwise masked correlations", {
  expect_equal( all_pairwise_correlations(v1, v2, 3, 3, list(c(1,5), c(6,10))),
                c(cor(v1[1:5], v2[1:5]), cor(v1[6:10], v2[6:10]), cor(v1, v2)) )

  expect_equal( all_pairwise_correlations(v1, v2, 1, 3, list(c(1,5), c(6,10))),
                c(cor(v1[1:5], v2[1:5]), NA, NA) )
})

test_that("mask set", {
  m <- matrix(nrow=3, ncol=3)
  m[1,2] <- cor(v1, v2)
  m[1,3] <- cor(v1, v3)
  m[2,3] <- cor(v2, v3)

  mc <- mean_correlation_for_mask_set(c(1,1,1), m)

  expect_equal(mc$average, mean(m, na.rm=T))
  expect_equal(mc$mask_combinations, c(1,1,1))

})

test_that("successfully computing all pairwise masked correlations", {
  d <- all_correlations( list(v1, v2), list(c(1,5), c(6,10)))
  expect_equal( d$items(),
                list( list(key = 3, value = cor(v1, v2)),
                      list(key = 2, value = cor(v1[6:10], v2[6:10])),
                      list(key = 1, value = cor(v1[1:5], v2[1:5]))
                )
  )

  d <- all_correlations( list(v1, v2, v3), list(c(1,5), c(6,10)))

  v11 <- v1[1:5]
  v21 <- v2[1:5]
  v31 <- v3[1:5]

  expect_equal( d[[ c(3, 3, 3) ]], mean(c(cor(v1, v2), cor(v1, v3), cor(v2, v3))) )
  expect_equal( d[[ c(1, 1, 3) ]],
                mean(c(
                  mean(c(cor(v11, v21), cor(v11, v31), cor(v2, v3))),
                  mean(c(cor(v11, v21), cor(v1, v3), cor(v21, v31))),
                  mean(c(cor(v1, v2), cor(v11, v31), cor(v21, v31)))
                ))
  )
})

test_that("finding mask with most set bits", {
  expect_equal( which.most_set_bits(list( c(1,2,3), c(1,0,0) )), 1 )
  expect_error( which.most_set_bits(list( c(0,1,0), c(1,0,0) )) )
})

