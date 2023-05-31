#include <Rcpp.h>
#include <cmath>
#include <vector>

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
  std::vector<int> shadeVector(height * width);

  for(int col = 0; col < width; col++) {
    for(int row = 0; row < height; row++) {
      if (minAltitude < altitudes(row, col)) {
        shadeMatrix(row, col) = 1;
      } else {
        shadeMatrix(row, col) = 0;
      }
    }
  }

  return shadeMatrix;
}
