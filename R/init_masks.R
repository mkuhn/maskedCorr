#' Detect mask groups in a vector
#'
#' @param v Input vector
#' @param mask_ranges List of start, end tuples for each of the mask groups
#' @return A number in which the i-th bit corresponds to the presence of the i-th mask group,
#'  or NA if one of the mask groups is a mixture of valid numbers and NAs.
#' @examples
#' detect_mask(c(NA,NA,3:10), list(c(1,2), c(3,6), c(7,10)))
detect_mask <- function(v, mask_ranges) {
  mask <- 0
  for (i in 1:length(mask_ranges)) {
    a <- mask_ranges[[i]][1]
    b <- mask_ranges[[i]][2]
    N <- 1 + b - a
    n.na <- sum(is.na(v[a:b]))

    if (n.na == 0) {
      mask <- bitwOr(mask, 2^(i-1))
      mask <- bitwOr(mask, 2^(i-1))
    } else if (n.na != N) {
      return(NA)
    }
  }
  return(mask)
}

