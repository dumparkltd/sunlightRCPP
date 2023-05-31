#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// modulo but for doubles
// scribed from https://stackoverflow.com/a/53998265
double dmod(double x, double y) {
  return x - (int)(x/y) * y;
}

double deg2rad(double deg) {
  return deg / 180 * M_PI;
}

double rad2deg(double rad) {
  return rad * 180 / M_PI;
}

double getCurvatureCorrection(double distance, double radiusEarth, double anglePerM) {
  double totalAngle = anglePerM * distance; //  Unit degrees * distance
  return radiusEarth * (1 - cos(deg2rad(totalAngle)));
}

//' @export
// [[Rcpp::export]]
NumericMatrix get_altitudes_for_azimuth_cpp(
    NumericMatrix& dem,
    double azimuth,
    double gridConvergence,
    double resolution,
    bool correctCurvature
  ) {
  // util
  double radiusEarth = 637100; // in m
  double circumferenceEarth = 2 * M_PI * radiusEarth;
  double anglePerUnit = 360 / circumferenceEarth;

  // figure out row and column offset dx, dy for azimuth
  double azi = dmod((azimuth + gridConvergence), 360);
  // steps x and y as factor
  double dx = 1;
  double dy = 1;
  // double azi2 = dmod(azi, 45);
  // double dl = 1;
  // double ds = tan(deg2rad(azi2));
  // if (dmod(azi, 90) > 45) {
  //   ds = tan(deg2rad(45 - azi2));
  // }
  //
  //
  // if (azi <= 45) {
  //   dx1 = tan(deg2rad(azi));
  //   dy1 = -1;
  // } else if ((azi > 45) & (azi <= 90)) {
  //   dx1 = 1;
  //   dy1 = -1 * tan(deg2rad(90 - azi));
  // } else if ((azi > 90) & (azi <= 135)) {
  //   dx1 = 1;
  //   dy1 = tan(deg2rad(azi - 90));
  // } else if ((azi > 135) & (azi <= 180)) {
  //   dx1 = tan(deg2rad(180 - azi));
  //   dy1 = 1;
  // } else if  ((azi > 180) & (azi <= 225)) {
  //   dx1 = -1 * tan(deg2rad(azi - 180));
  //   dy1 = 1;
  // } else if  ((azi > 225) & (azi <= 270)) {
  //   dx1 = -1;
  //   dy1 = tan(deg2rad(270 - azi));
  // } else if  ((azi > 270) & (azi <= 315)) {
  //   dx1 = -1;
  //   dy1 = -1 * tan(deg2rad(azi - 270));;
  // } else if (azi > 315) {
  //   dx1 = -1 * tan(deg2rad(360 - azi));
  //   dy1 = -1;
  // }
  //
  // NNE
  if (azi <= 45) {
    dx = tan(deg2rad(azi));
  // NEE
  } else if ((azi > 45) & (azi <= 90)) {
    dy = tan(deg2rad(90 - azi));
  // SEE
  } else if ((azi > 90) & (azi <= 135)) {
    dy = tan(deg2rad(azi - 90));
  // SSE
  } else if ((azi > 135) & (azi <= 180)) {
    dx = tan(deg2rad(180 - azi));
  // SSW
  } else if  ((azi > 180) & (azi <= 225)) {
    dx = tan(deg2rad(azi - 180));
  // SWW
  } else if  ((azi > 225) & (azi <= 270)) {
    dy = tan(deg2rad(270 - azi));
  // NWW
  } else if  ((azi > 270) & (azi <= 315)) {
    dy = tan(deg2rad(azi - 270));
  // NNW
  } else if (azi > 315) {
    dx = tan(deg2rad(360 - azi));
  }

  // correct sign
  // N
  if (azi <= 90 && azi > 270) {
    dy = -1 * dy;
  }
  // W
  if (azi > 180) {
    dx = -1 * dx;
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

  for(int col = 0; col < width; col++) {
    Rcpp::checkUserInterrupt();
    for(int row = 0; row < height; row++) {
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
            if (correctCurvature) {
              elevDiffStep = elevDiffStep - getCurvatureCorrection(distanceStep, radiusEarth, anglePerUnit);
            }
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
