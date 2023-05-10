#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
double get_min_altitude_for_azimuth_cpp(
   NumericMatrix& dem,
   int col,
   int row,
   double maxElev,
   double dx,
   double dy,
   double dxy
) {

 return col * row;
}
