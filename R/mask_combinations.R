#' Given two vectors, compute correlations for all possible masks
#'
#' @param v1 Input vector 1
#' @param v2 Input vector 2
#' @param m1 Pre-computed mask for v1
#' @param m2 Pre-computed mask for v2
#' @param mask_ranges List of start, end tuples for each of the mask groups
#' @return A vector of correlations for all possible masks. If a given mask is in
#'  conflict with the input masks, the entry in the vector will contain \code{NA}
#'  in that position.
all_pairwise_correlations <- function(v1, v2, m1, m2, mask_ranges) {
  m <- bitwAnd(m1, m2)
  N <- 2**length(mask_ranges)-1

  result <- rep(NA, N)

  for (i in 1:N) {
    if (bitwAnd(i, m) == i) result[i] <- masked_corr(v1, v2, i, mask_ranges)
  }

  result
}

