#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericMatrix get_sunlight_for_altitudes_cpp(
    NumericMatrix& altitudes,
    double minAltitude
  ) {

  // remember shape
  int width = altitudes.ncol();
  int height = altitudes.nrow();

  // initialize result matrix
  NumericMatrix sunlightMatrix(height, width);

  for(int col = 0; col < width; col++) {
    for(int row = 0; row < height; row++) {
      if (minAltitude < altitudes(row, col)) {
        sunlightMatrix(row, col) = 0;
      } else {
        sunlightMatrix(row, col) = 1;
      }
    }
  }

  return sunlightMatrix;
}
