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

double getCurvatureCorrection(double distance) {
  // util
  int radiusEarth = 6371000; // in m
  // 2 * M_PI * radiusEarth;
  double anglePerUnit = 0.000009;
  // 360 / circumferenceEarth;
  double totalAngle = anglePerUnit * distance; //  Unit degrees * distance
  return radiusEarth * (1 - cos(deg2rad(totalAngle)));
}

//' @export
// [[Rcpp::export]]
NumericMatrix get_altitudes_for_azimuth_cpp(
    NumericMatrix& dem,
    double azimuth,
    double gridConvergence,
    double resolution,
    bool correctCurvature,
    double incFactor
  ) {

  // figure out row and column offset dx, dy for azimuth
  double azi = dmod((azimuth + gridConvergence), 360);
  // steps x and y as factor
  double dx = 1;
  double dy = 1;
  // figure out effective angle for step calculation
  double aziRel = dmod(azi,90);
  if (aziRel > 45) {
    aziRel = 90 - aziRel;
  }
  double dopp = tan(deg2rad(aziRel));

  // NNE
  if (azi <= 45) {
    dx = dopp;
    dy = -1;
  // NEE
  } else if ((azi > 45) & (azi <= 90)) {
    //    dx = 1;
    dy = dopp * -1;
  // SEE
  } else if ((azi > 90) & (azi <= 135)) {
    //  dx = 1;
    dy = dopp;
  // SSE
  } else if ((azi > 135) & (azi <= 180)) {
    dx = dopp;
    // dy = 1;
  // SSW
  } else if  ((azi > 180) & (azi <= 225)) {
    dx = dopp * -1;
    // dy = 1;
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
      double correction = 0;
      if (!NumericVector::is_na(elevationOrigin)) {
        // calculate maximum possible difference in elevation
        int step = 0;
        int stepFactor = 0;
        // traverse transect to find max altitude difference
        while (true) {
          stepFactor = step + pow(incFactor, step + 1) ;
          step++;
          // stepFactor = step;
          double distanceStep = dxy * stepFactor;
          int rowStep = row + round(dy * stepFactor);
          int colStep = col + round(dx * stepFactor);
          if (rowStep >= 0 && rowStep < height && colStep >= 0 && colStep < width) {
            double elevStep = dem(rowStep, colStep);
            double elevDiffStep = elevStep - elevationOrigin;
            if (elevDiffStep > 0) {
              // if (correctCurvature) {
              //   correction = getCurvatureCorrection(distanceStep);
              //   elevDiffStep = elevDiffStep - correction;
              // }
              if (elevDiffStep > 0) {
                // calculate angle
                double altitudeStep = rad2deg(atan(elevDiffStep / distanceStep));
                if (altitudeStep > altitudeMin) {
                  altitudeMin = altitudeStep;
                } else {
                  // check if higher altitude is feasible
                  double elevationMaxDiff = maxElev - correction - elevationOrigin;
                  double altitudeMax = rad2deg(atan(elevationMaxDiff / distanceStep));
                  if (altitudeMax < altitudeMin) {
                    break;
                  }
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
