#'@title Calculate minimum altitudes
#'
#'@description Calculates minimum altitudes for a DEM raster and a range of azimuths
#'
#'@param dem dem raster
#'@param azimuth_min lower bounds of azimuths to calculate
#'@param azimuth_max upper bounds of azimuths
#'@param settings a settings object
#'@import raster
#'@return list of min altitudes for range of azimuths
#'@export
#'
#'
calculateMinAltitudes = function(dem_raster, azimuth_min, azimuth_max, settings) {
  for (azimuth in seq(azimuth_min, azimuth_max, by = settings$azimuth_step)) {
    print(paste(Sys.time(), ' - ', 'calculating altitudes for azimuth: ', azimuth, sep=""))
    outFilename = paste(
      settings$out_dir,
      "altitudes_azimuth-", azimuth,
      "_res-", gsub("\\.", "-", as.character(settings$resolution_dem_target)),
      "_inc-", gsub("\\.", "-", as.character(settings$inc_factor)),
      ".tif",
      sep=""
    )
    # call CPP function get_altitudes_for_azimuth_cpp to calculate the minimum
    # altitudes for all cells in the dem and current azimuth
    # returns a NumericMatrix
    alt_azi = get_altitudes_for_azimuth_cpp(
      as.matrix(dem_raster),
      azimuth,
      settings$grid_convergence,
      settings$resolution_dem_target,
      settings$correct_curvature,
      settings$inc_factor
    )
    print(paste(Sys.time(), ' - ', 'writing raster for azimuth ', azimuth, ' to: ', outFilename, sep=""))

    rasterForAzimuth <- raster::raster(
      nrows = raster::nrow(dem_raster),
      ncols = raster::ncol(dem_raster),
      ext = raster::extent(dem_raster),
      res = raster::res(dem_raster),
      crs = raster::crs(dem_raster),
      vals = alt_azi
    )
    raster::writeRaster(
      rasterForAzimuth,
      filename=outFilename),
      format="GTiff",
      overwrite=TRUE
    )
    print(paste(Sys.time(), ' - ', 'DONE: writing raster for azimuth: ', azimuth, sep=""))
    if (settings$cut_vertically == TRUE) {
      print(paste(Sys.time(), ' - ', 'writing raster stripes for azimuth: ', azimuth, sep=""))
      xres = raster::xres(rasterForAzimuth) # in px
      stripe_w_px = stripeWidth/xres # in p
      num_stripes <- ceiling(ncol(rasterForAzimuth) / stripe_w_px)
      stripe_boundaries <- seq(from = 1, to = ncol(rasterForAzimuth), by = stripe_w_px)
      stripe_boundaries <- c(stripe_boundaries, ncol(rasterForAzimuth))
      stripes <- list()
      for (i in 1:(num_stripes)) {
        start_col <- stripe_boundaries[i]
        end_col <- stripe_boundaries[i+1] - 1
        stripes[[i]] <- raster::crop(rasterForAzimuth, raster::extent(rasterForAzimuth, 1, raster::nrow(rasterForAzimuth), start_col, end_col))
      }
      for (i in 1:(num_stripes)) {
        stripe <- stripes[[i]]
        outFilename = paste(
          "altitudes_azimuth-", azimuth,
          "_stripe-", i,
          ".tif",
          sep=""
        )
        # print(outFilename)
        # print(result[[azimuth]])

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
}
