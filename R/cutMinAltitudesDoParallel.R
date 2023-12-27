#'@title Calculate minimum altitudes
#'
#'@description Calculates minimum altitudes for a DEM raster and a range of azimuths
#'
#'@param dem dem raster
#'@param azimuth_min lower bounds of azimuths to calculate
#'@param azimuth_max upper bounds of azimuths
#'@param settings a settings object
#'@import raster
#'@import doParallel
#'@import foreach
#'@import stringr
#'@return list of min altitudes for range of azimuths
#'@export
#'
#'
cutMinAltitudesDoParallel = function(
    altitudeDir,
    altitudeFilePattern,
    altitudeFilePlaceholder = 'azi',
    outDir,
    azimuthMin,
    azimuthMax,
    azimuthStep,
    stripeNo
) {
  cores = parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  foreach (azimuth = seq(azimuthMin, azimuthMax, by = azimuthStep)) %dopar% {
    library(raster)
    library(stringr)
    # load altitudes raster
    altFile <- stringr::str_replace(altitudeFilePattern, '\\{azi\\}', as.character(azimuth))
    altFileAndPath <- paste(altitudeDir, altFile, sep="")
    rasterForAzimuth = raster::raster(altFileAndPath)

    stripe_width = ceiling(ncol(rasterForAzimuth) / stripeNo)

    stripe_boundaries <- seq(from = 1, to = ncol(rasterForAzimuth), by = stripe_width)
    stripe_boundaries <- c(stripe_boundaries, ncol(rasterForAzimuth))
    stripes <- list()
    for (i in 1:(stripeNo)) {
      start_col <- stripe_boundaries[i]
      end_col <- stripe_boundaries[i+1] - 1
      stripe <- raster::crop(rasterForAzimuth, raster::extent(rasterForAzimuth, 1, raster::nrow(rasterForAzimuth), start_col, end_col))
      outFilename = paste(
        stringr::str_replace(altFile, '.tif', '_'),
        "stripe-", i,
        ".tif",
        sep=""
      )
      # print(paste(Sys.time(), ' - ', 'writing raster for stripe: ', i, ' to: ', outDir, outFilename,  sep=""))

      raster::writeRaster(
        stripe,
        filename=paste(outDir, outFilename, sep=""),
        format="GTiff",
        overwrite=TRUE
      )
    }
    print(paste(Sys.time(), ' - ', 'DONE: writing raster stripes for azimuth: ', azimuth, sep=""))
  }
}
