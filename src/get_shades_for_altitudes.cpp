#include <Rcpp.h>
#include <cmath>
#include <vector>
// #include <algorithm>    // std::transform

using namespace Rcpp;

// function declaration
// int isShade(double alt, double minAlt) {
//   if (minAlt < alt) {
//     return 1;
//   } else {
//     return 0;
//   }
// };


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

  for(int col = 0; col < width; col++) {
    for(int row = 0; row < height; row++) {
      if (minAltitude < altitudes(row, col)) {
        shadeMatrix(row, col) = 1;
      } else {
        shadeMatrix(row, col) = 0;
      }
    }
  }
  // alt implementation
  //
  // std::transform(
  //   altitudes.begin(),
  //   altitudes.end(),
  //   shadeMatrix.begin(),
  //   [minAltitude] (int value) { if (minAltitude < value) { return 1; } else { return 0; }}
  // );

  return shadeMatrix;
}
