#' Detect mask groups in a vector
#'
#' @param v Input vector
#' @param mask_ranges List of start, end tuples for each of the mask groups
#' @return A number in which the i-th bit corresponds to the absence of NAs
#'  in the i-th mask group,
#' @examples
#' detect_mask(c(NA,NA,3:10), list(c(1,2), c(3,6), c(7,10)))
detect_mask <- function(v, mask_ranges) {
  mask <- 0
  for (i in 1:length(mask_ranges)) {
    a <- mask_ranges[[i]][1]
    b <- mask_ranges[[i]][2]

    if (all(!is.na(v[a:b]))) {
      mask <- bitwOr(mask, 2^(i-1))
      mask <- bitwOr(mask, 2^(i-1))
    }
  }
  return(mask)
}

