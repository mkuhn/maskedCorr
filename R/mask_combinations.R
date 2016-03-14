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



mean_correlation_for_mask_set <- function(N, mask, correlations) {
  count <- 0
  corr_sum <- 0

  mask_combinations <- integer(N*(N-1)/2)

  for (i in 1:(N-1)) {
    target1 <- mask[i]
    for (j in (i+1):N) {
      target2 <- mask[j]
      target <- bitwAnd(target1, target2)

      if (target > 0) {
        count <- count + 1
        mask_combinations[count] <- target
        corr <- correlations[[i, j]][target]
        if (is.na(corr)) return()
        corr_sum <- corr_sum + corr
      }
    }
  }

  if (count == 0) return()

  list(average = corr_sum / count, mask_combinations = sort(mask_combinations))
}



all_correlations <- function(vs, mask_ranges) {
  N <- length(vs)
  Nbits <- length(mask_ranges)
  Nmax_mask <- 2**Nbits
  ms <- sapply(vs, detect_mask, mask_ranges = mask_ranges)

  correlations <- matrix(list(), nrow=N, ncol=N)

  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      correlations[[i, j]] <- all_pairwise_correlations(vs[[i]], vs[[j]], ms[[i]], ms[[j]], mask_ranges)
    }
  }

  mask <- increment_mask(rep(0, N), Nmax_mask)

  while (!is.na(mask[1])) {

    if (!contains_singletons(mask, Nbits)) {
      l <- mean_correlation_for_mask_set(N, mask, correlations)

      if (!is.null(l)) {
        print("-----")
        print(mask)
        print(l)
      }
    }

    mask <- increment_mask(mask, Nmax_mask)
  }
}

