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


#' Given a precomputed matrix of masked correlations, gather correlations
#' for the specified mask set.
#'
#' @param masks vector of masks (length N) from which to take correlations
#' @param correlations NxN matrix of correlations, where each matrix element is a vector
#'  with entries for each masked correlation
#' @param masks_filter optional: set of masks (of the comparison, not the input mask) for which
#'  to return correlations, supply as integer-encoded mask (not as vector)
#' @param Nbits optional: number of bits, must be supplied if masks_filter is given
#' @return A list of two elements: the average correlation, and a sorted vector
#'  of the encountered combination of masks. It is possible that the given masks and
#'  correlations are partially not compatible, Then, the average will be computed
#'  based on those mask combinations that contain a correlation. (E.g. in the mask triple
#'  "A, AB, B" the average will be computed from two correlations (A/A, B/B), as it is
#'  not possible to calculate a correlation between A and B.)
mean_correlation_for_mask_set <- function(masks, correlations, Nbits, masks_filter = NULL) {
  count <- 0
  corr_sum <- 0

  N <- length(masks)

  mask_combinations <- masks_to_target(masks)
  sorted_mask_combinations <- masks_to_int(sort(mask_combinations), Nbits)

  if (!is.null(masks_filter) && !(sorted_mask_combinations %in% masks_filter))
    return()

  bit <- 1
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      target <- mask_combinations[bit]
      bit <- bit+1

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

  list(average = corr_sum / count, mask_combinations = sorted_mask_combinations)
}


#' Compute correlations for all possible mask combinations given the input vectors.
#'
#' @param vs Input vectors
#' @param mask_ranges List of start, end tuples for each of the mask groups
#' @param masks_filter optional: set of masks (of the comparison, not the input mask) for which
#'  to return correlations, supply as integer-encoded mask (not as vector)
#' @return A dictionary of mask combinations and the respective correlations.
#' @export
all_correlations <- function(vs, mask_ranges, masks_filter = NULL) {
  N <- length(vs)
  Nbits <- length(mask_ranges)

  ms <- sapply(vs, detect_mask, mask_ranges = mask_ranges)

  correlations <- matrix(list(), nrow=N, ncol=N)

  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      correlations[[i, j]] <- all_pairwise_correlations(vs[[i]], vs[[j]], ms[[i]], ms[[j]], mask_ranges)
    }
  }

  masks <- rep(1, N)

  d <- dict::numvecdict()

  while (1) {
    l <- mean_correlation_for_mask_set(masks, correlations, Nbits, masks_filter)
    if (!is.null(l)) {
      d$append_number(l$mask_combinations, l$average)
    }
    masks <- increment_mask(masks, Nbits, T, T)
    if (is.na(masks[1])) break
  }

  d$inplace_mean()
  d
}


#' Compute correlations for the largest possible mask given the input vectors,
#'
#' @param vs Input vectors
#' @param mask_ranges List of start, end tuples for each of the mask groups
#' @return A list of the correlation and the encountered mask.
#' @export
best_masked_correlation <- function(vs, mask_ranges) {

  N <- length(vs)
  masks <- sapply(vs, detect_mask, mask_ranges = mask_ranges)

  if (any(is.na(masks))) return()

  correlations <- matrix(list(), nrow=N, ncol=N)

  for (i in 1:(N-1)) {
    m1 <- masks[i]
    for (j in (i+1):N) {
      m2 <- masks[j]
      m <- bitwAnd(m1, m2)
      # generate a vector here so that we can re-use the standard function
      # which expects pre-computed correlations for all possible masks
      correlations[[i, j]] <- numeric(m)
      correlations[[i, j]][m] <- masked_corr(vs[[i]], vs[[j]], m, mask_ranges)
    }
  }

  mean_correlation_for_mask_set(masks, correlations, length(mask_ranges))
}

#' Find mask with most keys set
#'
#' @param masks List of masks
#' @return The items with the most set bits. Will raise an error if more
#'  than one items have the same number of set bits
#' @export
which.most_set_bits <- function(masks) {
  counts <- sapply(masks, count_set_bits)
  m <- max(counts)
  result <- which(counts == m)
  if (length(result) > 1) stop(paste(length(result), "items have", m, "set bits!"))
  result
}


