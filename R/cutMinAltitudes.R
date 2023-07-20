#'@title Calculate minimum altitudes
#'
#'@description Calculates minimum altitudes for a DEM raster and a range of azimuths
#'
#'@param dem dem raster
#'@param azimuth_min lower bounds of azimuths to calculate
#'@param azimuth_max upper bounds of azimuths
#'@param settings a settings object
#'@import raster
#'@import stringr
#'@return list of min altitudes for range of azimuths
#'@export
#'
#'
cutMinAltitudes = function(
    altitudeDir,
    altitudeFilePattern,
    altitudeFilePlaceholder = 'azi',
    outDir,
    azimuthMin,
    azimuthMax,
    azimuthStep,
    stripeWidth = 10000
) {


  for (azimuth in seq(azimuthMin, azimuthMax, by = azimuthStep)) {
    # load altitudes raster
    altFile <- stringr::str_replace(altitudeFilePattern, '\\{azi\\}', as.character(azimuth))
    altFileAndPath <- paste(altitudeDir, altFile, sep="")
    print(paste(Sys.time(), " - ", "loading altitudes file: ", altFile, " from: ", altFileAndPath, "for azimuth: ", azimuth , sep=""))
    rasterForAzimuth = raster::raster(altFileAndPath)

    # horizontal resolution
    xres = raster::xres(rasterForAzimuth) # in px
    stripe_w_px = stripeWidth/xres # in p
    num_stripes <- ceiling(ncol(rasterForAzimuth) / stripe_w_px)


    print(paste(Sys.time(), ' - ', 'writing ', num_stripes, ' raster stripes for azimuth: ', azimuth, sep=""))

    stripe_boundaries <- seq(from = 1, to = ncol(rasterForAzimuth), by = stripe_w_px)
    stripe_boundaries <- c(stripe_boundaries, ncol(rasterForAzimuth))
    stripes <- list()
    for (i in 1:(num_stripes)) {
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
