#include <Rcpp.h>
using namespace Rcpp;

//' Test if any bit in the input masks is set exactly once
// [[Rcpp::export]]
bool contains_singletons(IntegerVector vs, int N_bits) {

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

