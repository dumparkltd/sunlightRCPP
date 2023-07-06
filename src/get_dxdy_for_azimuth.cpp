#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// modulo but for doubles
// scribed from https://stackoverflow.com/a/53998265
double _dmod(double x, double y) {
  return x - (int)(x/y) * y;
}

double _deg2rad(double deg) {
  return deg / 180 * M_PI;
}

double _rad2deg(double rad) {
  return rad * 180 / M_PI;
}

//' @export
// [[Rcpp::export]]
NumericVector get_dxdy_for_azimuth_cpp(
    NumericMatrix& dem,
    double azimuth,
    double resolution
  ) {

  // figure out row and column offset dx, dy for azimuth
  double azi = azimuth;
  // steps x and y as factor
  double dx = 0;
  double dy = 0;
  // figure out effective angle for step calculation
  double aziRel = _dmod(azi,90);
  if (aziRel > 45) {
    aziRel = 90 - aziRel;
  }
  double dopp = tan(_deg2rad(aziRel));

  // NNE
  if (azi <= 45) {
    dx = dopp;
    dy = -1;
  // NEE
  } else if ((azi > 45) & (azi <= 90)) {
    dx = 1;
    dy = dopp * -1;
  // SEE
  } else if ((azi > 90) & (azi <= 135)) {
    dx = 1;
    dy = dopp;
  // SSE
  } else if ((azi > 135) & (azi <= 180)) {
    dx = dopp;
    dy = 1;
  // SSW
  } else if  ((azi > 180) & (azi <= 225)) {
    dx = dopp * -1;
    dy = 1;
  // SWW
  } else if  ((azi > 225) & (azi <= 270)) {
    dx = -1;
    dy = dopp;
  // NWW
  } else if  ((azi > 270) & (azi <= 315)) {
    dx = -1;
    dy = dopp * -1;
  // NNW
  } else if (azi > 315) {
    dx = dopp * -1;
    dy = -1;
  }
  double dxy = sqrt(pow(dx, 2) + pow(dy, 2)) * resolution;
  NumericVector dxdy = {dx, dy, dxy};

  return dxdy;
}
