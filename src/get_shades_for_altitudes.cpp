#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericMatrix get_shades_for_altitudes_cpp(
    NumericMatrix& altitudes,
    double minAltitude
  ) {

  // remember shape
  int width = altitudes.ncol();
  int height = altitudes.nrow();

  // initialize result matrix
  NumericMatrix shadeMatrix(height, width);

  for(int row = 0; row < height; row++) {
    Rcpp::checkUserInterrupt();
    for(int col = 0; col < width; col++) {
      Rcpp::checkUserInterrupt();
      if (minAltitude < altitudes(row, col)) {
        shadeMatrix(row, col) = 1;
      } else {
        shadeMatrix(row, col) = 0;
      }
    }
  }

  return shadeMatrix;
}
