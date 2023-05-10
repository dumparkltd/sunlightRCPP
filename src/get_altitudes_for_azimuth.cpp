#include <Rcpp.h>

using namespace Rcpp;

// modulo but for doubles
// scribed from https://stackoverflow.com/a/53998265
double dmod(double x, double y) {
  return x - (int)(x/y) * y;
}

double deg2rad(double deg) {
  return deg / 180 * 3.14159265;
}

double rad2deg(double rad) {
  return rad * 180 / 3.14159265;
}

//' @export
// [[Rcpp::export]]
NumericMatrix get_altitudes_for_azimuth_cpp(
    NumericMatrix& dem,
    double azimuth,
    double gridConvergence,
    double resolution
  ) {
  // figure out row and column offset dx, dy for azimuth
  double azi = dmod((azimuth + gridConvergence), 360);
  double dx = 1;
  double dy = 1;
  // TODO there must be a better way...
  if (azi <= 45) {
    double dOpposite = tan(deg2rad(azi));
    dx = dOpposite;
    dy = -1;
  } else if (azi > 45 & azi <= 90) {
    double dOpposite = tan(deg2rad(90 - azi));
    dy = -1 * dOpposite;
  } else if (azi > 90 & azi <= 135) {
    double dOpposite = tan(deg2rad(azi - 90));
    dy = dOpposite;
  } else if (azi > 135 & azi <= 180) {
    double dOpposite = tan(deg2rad(180 - azi));
    dx = dOpposite;
  } else if  (azi > 180 & azi <= 225) {
    double dOpposite = tan(deg2rad(azi - 180));
    dx = -1 * dOpposite;
  } else if  (azi > 225 & azi <= 270) {
    double dOpposite = tan(deg2rad(270 - azi));
    dx = -1;
    dy = dOpposite;
  } else if  (azi > 270 & azi <= 315) {
    double dOpposite = tan(deg2rad(azi - 270));
    dx = -1;
    dy = -1 * dOpposite;
  } else if  (azi > 315) {
    double dOpposite = tan(deg2rad(360 - azi));
    dx = -1 * dOpposite;
    dy = -1;
  }
  // dxy = distance of sampling steps in m
  double dxy = sqrt(pow(dx, 2) + pow(dy, 2)) * resolution;

  // remember shape
  int width = dem.ncol();
  int height = dem.nrow();

  // initialize result matrix
  NumericMatrix minAltitudeMatrix(height, width);

  // get max height from dem
  double maxElev = max(dem);

  for(int row = 0; row < height; row++) {
    Rcpp::checkUserInterrupt();
    for(int col = 0; col < width; col++) {
      Rcpp::checkUserInterrupt();
      double elevationOrigin = dem(row, col);
      double altitudeMin = 0;
      if (elevationOrigin != 0) {
        // calculate maximum possible difference in elevation
        double elevationMaxDiff = maxElev - elevationOrigin;
        int step = 0;
        // traverse transect to find max altitude difference
        while (true) {
          step++;
          double distanceStep = dxy * step;
          int rowStep = row + round(dy * step);
          int colStep = col + round(dx * step);
          if (rowStep >= 0 && rowStep < height && colStep >= 0 && colStep < width) {
            double elevStep = dem(rowStep, colStep);
            double elevDiffStep = elevStep - elevationOrigin;
            if (elevDiffStep > 0) {
              // calculate angle
              double altitudeStep = rad2deg(atan(elevDiffStep / distanceStep));
              if (altitudeStep > altitudeMin) {
                altitudeMin = altitudeStep;
              } else {
                // check if higher altitude is feasible
                double altitudeMax = rad2deg(atan(elevationMaxDiff / distanceStep));
                  if (altitudeMax < altitudeMin) {
                    break;
                  }
              }
            }
          } else {
            // break if out of bounds
            break;
          }
        }
      }
      minAltitudeMatrix(row, col) = altitudeMin;
    }
  }

  return minAltitudeMatrix;
}
