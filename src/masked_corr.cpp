#include <Rcpp.h>
using namespace Rcpp;

//' Calculate correlation only for ranges where the mask bit has been set
//'
//' @param v1 Input vector 1
//' @param v2 Input vector 2
//' @param mask Mask, each bit that is set will be included in the correlation
//' @param mask_ranges List of start, end tuples for each of the mask groups
//' @return The Pearson correlation between the designated parts of the input vectors
//' @export
// [[Rcpp::export]]
double masked_corr(NumericVector v1, NumericVector v2, int mask, List mask_ranges) {

  const int n_mask = mask_ranges.size();

  int n = 0;

  // determine mean
  double sum1 = 0;
  double sum2 = 0;

  for (int i = 0; i < n_mask; i++) {
    if ((1 << i) & mask) {
      NumericVector range = as<NumericVector>(mask_ranges[i]);

      const int start = range[0]-1;
      const int end = range[1];

      n += end - start;

      for (int j = start; j < end; j++) {
        sum1 += v1[j];
        sum2 += v2[j];
      }
    }
  }

  const double mean1 = sum1 / n;
  const double mean2 = sum2 / n;

  // calculate correlation
  double numerator = 0;
  double denominator1 = 0;
  double denominator2 = 0;

  for (int i = 0; i < n_mask; i++) {
    if ((1 << i) & mask) {
      NumericVector range = as<NumericVector>(mask_ranges[i]);

      const int start = range[0]-1;
      const int end = range[1];

      for (int j = start; j < end; j++) {
        const double d1 = v1[j] - mean1;
        const double d2 = v2[j] - mean2;

        numerator += d1 * d2;
        denominator1 += d1 * d1;
        denominator2 += d2 * d2;
      }
    }
  }

  return numerator / sqrt(denominator1*denominator2);
}

