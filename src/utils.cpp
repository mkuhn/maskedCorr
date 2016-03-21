#include <Rcpp.h>
using namespace Rcpp;

//' Test if any bit in the input masks is set exactly once
// [[Rcpp::export]]
bool contains_singleton(IntegerVector vs, int N_bits) {

  const int N = vs.size();

  for (int bit = 0; bit < N_bits; bit++) {
    const int mask = 1 << bit;
    int count = 0;
    for (int i = 0; i < N; i++) {
      count += (0 < (vs[i] & mask));
    }
    if (count == 1) return true;
  }

  return false;
}

//' count the number of bits set
//' @export
// [[Rcpp::export]]
int count_set_bits(IntegerVector vs) {

  const int N = vs.size();
  int count = 0;

  for (int i = 0; i < N; i++) {
    int v = vs[i];
    while (v > 0) {
      count += 1 & v;
      v = v >> 1;
    }
  }

  return count;
}

//' Convert the mask to an integer for easier handling with dplyr
//' NOTE! We have at most 6 elements in the mask, and 4 bits = 24 bits.
//' so we're safe.
//' @param masks Integer vector of masks
//' @param bits_per_mask Number of bits in each mask
//' @export
// [[Rcpp::export]]
int masks_to_int(IntegerVector masks, int bits_per_mask) {
  const int N = masks.size();
  if (std::numeric_limits<int>::digits < N * bits_per_mask)
    stop("Number of bits too large!");

  int result = 0;
  for (int i = 0; i < N; i++)
    result += masks[i] << (bits_per_mask*i);

  return result;
}

//' Split from integer value into masks
//' @param int_masks encoded masks
//' @param bits_per_mask Number of bits in each mask
//' @param N_masks Number of masks
//' @export
// [[Rcpp::export]]
IntegerVector int_to_masks(int int_masks, int bits_per_mask, int N_masks) {

  IntegerVector result = IntegerVector(N_masks);

  const int filter_mask = (1 << bits_per_mask) - 1;

  for (int i = 0; i < N_masks; i++) {
    result[i] = int_masks & filter_mask;
    int_masks = int_masks >> bits_per_mask;
  }

  return result;
}

//' Cycle through all possible mask combinations by incrementing the mask
//'
//' @param mask Input mask
//' @param N threshold for setting one part of the mask to zero and moving to next
//' @param skip_singletons If set, do not return singletons
//' @param skip_zeros If set, do not return masks where any submask is 0
//' @return The incremented mask, or \code{NA} if all combinations have been reached
// [[Rcpp::export]]
IntegerVector increment_mask(IntegerVector mask, int N_bits,
                             bool skip_singletons, bool skip_zeros) {

  const int N_mask = mask.size();
  const int N = 1 << N_bits;

  while (1) {

    bool found_singleton = false;

    for (int i = 0; i < N_mask; i++) {
      const int a = mask[i] + 1;

      if (skip_zeros && a == 1)
        stop("Mask needs to be initialized to non-zero values for skip_zeros!");

      if (a < N) {
        mask[i] = a;
        if (!skip_singletons || !contains_singleton(mask, N_bits)) {
          return mask;
        } else {
          found_singleton = true;
          break;
        }
      } else {
        mask[i] = skip_zeros;
      }
    }

    if (!found_singleton) {
      Rcpp::IntegerVector v;
      v.push_back(NA_INTEGER);
      return v;
    }
  }
}


//' Generate vector of masks containing all intersections of bits
//' @param masks input set of masks
//' @return same as upper.tri of outer(masks, masks), bitwAnd)
// [[Rcpp::export]]
IntegerVector masks_to_target(IntegerVector masks) {
  const int N = masks.size();
  IntegerVector result = IntegerVector( N*(N-1)/2 );

  int bit = 0;

  for (int i = 0; i < (N-1); i++) {
    for (int j = i+1; j < N; j++ ) {
      result[bit++] = masks[i] & masks[j];
    }
  }

  return result;
}
