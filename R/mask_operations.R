#' Cycle through all possible mask combinations by incrementing the mask
#'
#' @param mask Input mask
#' @param N threshold for setting one part of the mask to zero and moving to next
#' @return The incremented mask, or \code{NA} if all combinations have been reached
increment_mask <- function(mask, N) {
  for (i in 1:length(mask)) {
    a <- mask[i] + 1
    if (a < N) {
      mask[i] <- a
      return(mask)
    }
    mask[i] <- 0
  }
  NA
}
