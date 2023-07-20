// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
#include <cmath>
#include <vector>
// #include <algorithm>    // std::transform

using namespace Rcpp;
using namespace RcppParallel;


struct ParallelWorker : public Worker {
  const NumericMatrix& altitudes;
  Rcpp::NumericMatrix sunlight;
  const double minAltitude;
  const int height;

  ParallelWorker(
    const NumericMatrix& altitudes,
    Rcpp::NumericMatrix sunlight,
    const double minAltitude,
    const int height
  ) :
    altitudes(altitudes),
    sunlight(sunlight),
    minAltitude(minAltitude),
    height(height){}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t col = begin; col < end; col++) {
      for(int row = 0; row < height; row++) {
        if (minAltitude < altitudes(row, col)) {
          sunlight(row, col) = 0;
        } else {
          sunlight(row, col) = 1;
        }
      }
    }
  }
};

//' @export
// [[Rcpp::export]]
NumericMatrix get_sunlight_for_altitudes_p_cpp(
    NumericMatrix& altitudes,
    double minAltitude
  ) {

  // remember shape
  int width = altitudes.ncol();
  int height = altitudes.nrow();
  // initialize result matrix
  Rcpp::NumericMatrix sunlightMatrix(height, width);

  ParallelWorker parallelWorker(
      altitudes, // input matrix
      sunlightMatrix, // output matrix
      minAltitude,
      height
  );
  // parallelise columns
  parallelFor(0, width, parallelWorker);

  return sunlightMatrix;
}
