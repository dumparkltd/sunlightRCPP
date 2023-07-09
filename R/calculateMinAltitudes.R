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
calculateMinAltitudes = function(dem, azimuth_min, azimuth_max, settings) {
  azimuth_step = settings$azimuth_step
  result <- list()
  for (azimuth in seq(azimuth_min, azimuth_max, by = azimuth_step)) {
    print(paste(Sys.time(), ' - ', 'calculating altitudes for azimuth: ', azimuth, sep=""))
    # call CPP function get_altitudes_for_azimuth_cpp to calculate the minimum
    # altitudes for all cells in the dem and current azimuth
    # returns a NumericMatrix
    alt_azi = get_altitudes_for_azimuth_cpp(
      as.matrix(dem),
      azimuth,
      settings$grid_convergence,
      settings$resolution_dem_target,
      settings$correct_curvature,
      settings$inc_factor
    )
    result[[as.character(azimuth)]] = alt_azi
  }

  return(result)
}
