// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;
using namespace RcppParallel;

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

struct ParallelWorker : public Worker {
  const NumericMatrix& input_dem;
  Rcpp::NumericMatrix output_matrix;
  const double dx;
  const double dy;
  const double dxy;
  const int height;
  const int width;
  const double maxElev;
  const double inc_factor;

  ParallelWorker(
    const NumericMatrix& input,
    Rcpp::NumericMatrix output,
    const double dx,
    const double dy,
    const double dxy,
    const int height,
    const int width,
    const double maxElev,
    const double inc_factor
  ) :
    input_dem(input),
    output_matrix(output),
    dx(dx),
    dy(dy),
    dxy(dxy),
    height(height),
    width(width),
    maxElev(maxElev),
    inc_factor(inc_factor) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t col = begin; col < end; col++) {
      for(int row = 0; row < height; row++) {
        double elevationOrigin = input_dem(row, col);
        double altitudeMin = 0;
        if (!NumericVector::is_na(elevationOrigin)) {
          // calculate maximum possible difference in elevation
          int step = 0;
          int stepFactor = 0;
          // traverse transect to find max altitude difference
          while (true) {
            stepFactor = step + pow(inc_factor, step + 1) ;
            step++;
            // stepFactor = step;
            double distanceStep = dxy * stepFactor;
            int rowStep = row + round(dy * stepFactor);
            int colStep = col + round(dx * stepFactor);
            if (rowStep >= 0 && rowStep < height && colStep >= 0 && colStep < width) {
              double elevStep = input_dem(rowStep, colStep);
              double elevDiffStep = elevStep - elevationOrigin;
              if (elevDiffStep > 0) {
                if (elevDiffStep > 0) {
                  // calculate angle
                  double altitudeStep = rad2deg(atan(elevDiffStep / distanceStep));
                  if (altitudeStep > altitudeMin) {
                    altitudeMin = altitudeStep;
                  } else {
                    // check if higher altitude is feasible
                    double elevationMaxDiff = maxElev - elevationOrigin;
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
        output_matrix(row, col) = altitudeMin;
      }
    }
  }
};


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
  Rprintf("get_altitudes_for_azimuth_cpp (%f) \n", azimuth);
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
  double maxElev = max(dem);
  // initialize result matrix
  Rcpp::NumericMatrix minAltitudeMatrix(height, width);
  ParallelWorker parallelWorker(
      dem, // input matrix
      minAltitudeMatrix, // output matrix
      dx,
      dy,
      dxy,
      height,
      width,
      maxElev,
      incFactor
  );
  // parallelise columns
  Rprintf("parallelWorker start (%i) \n", width);

  parallelFor(0, width, parallelWorker);

  Rprintf("parallelWorker stop (%i) \n", width);
  return minAltitudeMatrix;
}
