#'@title Calculate Horizons Map Main
#'
#'@description Calculates horizons map for an elevation matrix
#'
#'
#'@useDynLib sunlightRCPP, .registration = TRUE
#'@importFrom Rcpp evalCpp
#'@import raster
#'@import sp
#'@import suncalc
#'@export

sunlight = function() {
  print(Sys.time())
  settings <- list(
    azimuth_min = 130,
    azimuth_max = 130,
    azimuth_step = 10,
    resolution_dem_target = 100,
    grid_convergence = 1.4804503109283933,
    correct_curvature = FALSE
  )
  print("loading dem...")
  # load raster
  dem = raster::raster("~/projects/INRAE/data/raster1.tif")


  print("re-sampling dem...")
  # re-sample original raster
  dem_at_res <- raster::aggregate(
    dem,
    fact = ceiling(settings$resolution_dem_target / xres(dem))
  )

  azimuth_min <- settings$azimuth_min
  azimuth_max <- settings$azimuth_max

  if (is.null(azimuth_min) | is.null(azimuth_max)){

    print("determining azimuth range...")
    # figure out min/max azimuth
    latlon <- as.data.frame(
      sp::spTransform(
        raster::xyFromCell(dem_at_res,c(1),spatial=TRUE),
        CRS("+proj=longlat")
      )
    )
    lon = latlon$x
    lat = latlon$y

    date_longest_N = as.Date("2023-06-21")
    date_longest_S = as.Date("2023-12-21")
    # get azimuth boundaries
    if (lat > 0) {
      # https://rdrr.io/cran/suncalc/man/getSunlightTimes.html
      sltimes <- suncalc::getSunlightTimes(
        date = date_longest_N,
        lat = lat,
        lon = lon,
        keep = c("sunrise", "sunset"),
        tz = "UTC"
      )
    } else {
      sltimes <- suncalc::getSunlightTimes(
        date = date_longest_S,
        lat = lat,
        lon = lon,
        keep = c("sunrise", "sunset"),
        tz = "UTC"
      )
    }
    # in radians
    # "sun azimuth in radians (direction along the horizon, measured
    # from south to west), e.g. 0 is south and Math.PI * 3/4 is northwest"
    # https://rdrr.io/cran/suncalc/man/getSunlightPosition.html
    slp_sunrise <- suncalc::getSunlightPosition(
      date= sltimes$sunrise,
      lat = lat,
      lon = lon,
      keep = c( "azimuth")
    )
    slp_sunset <- suncalc::getSunlightPosition(
      date= sltimes$sunset,
      lat = lat,
      lon = lon,
      keep = c( "azimuth")
    )
    # convert to degrees, North to East, round up/down
    # WARNING not tested for S hemisphere
    azimuth_min <- floor(rad2deg(slp_sunrise$azimuth) + 180)
    azimuth_max <- ceiling(rad2deg(slp_sunset$azimuth) + 180)

    # round azimuths
    res_azimuths = settings$azimuth_step
    azimuth_min <- ceiling(azimuth_min / res_azimuths) * res_azimuths
    azimuth_max <- floor(azimuth_max / res_azimuths) * res_azimuths
    print(paste(Sys.time(), ' - ', 'calculating altitudes for azimuths: ', azimuth_min, ':', azimuth_max, '...', sep=""))
  }
  # get altitude raster layers for each azimuth
  result = calculateMinAltitudes(
    dem_at_res,
    azimuth_min,
    azimuth_max,
    settings
  )

  print(Sys.time())
  return(result)
  # print(horizons_raster)
}
