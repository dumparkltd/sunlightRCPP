#'@title Calculate altitude angles for a dem and range of azimuths
#'
#'@description Calculates altitude angles for each cell of a dem and a range of sun azimuths above which the cell potentially receives direct sunlight
#'
#'
#'@useDynLib sunlightRCPP, .registration = TRUE
#'@importFrom Rcpp evalCpp
#'@import raster
#'@import sp
#'@import suncalc
#'@export

precalcAltitudeDistances = function(
    dem = "dem2.tif",
    demDir = "~/projects/INRAE/data/",
    outDir = "~/projects/INRAE/data/altitudes/RCPP/",
    azimuthStep = 1,
    azimuthMin = NULL,
    azimuthMax = NULL,
    targetResolution = 12.5,
    gridConvergence = 0, # WGS84
    correctCurvature = FALSE
) {
  # gridConvergence = 1.4804503109283933 for lambert conformal conic
  # load raster
  demFileAndPath = paste(demDir, dem, sep="")
  print(paste(Sys.time(), " - ", "loading dem: ", dem, " from: ", demFileAndPath, sep=""))
  dem_original = raster::raster(demFileAndPath)

  if (raster::xres(dem_original) != targetResolution) {
    print(paste("re-sampling dem: ", targetResolution, sep = ""))
    # re-sample original raster
    dem_at_res <- raster::aggregate(
      dem_original,
      fact = targetResolution / raster::xres(dem_original)
    )
  } else {
    dem_at_res <- dem_original
  }


  azimuth_min <- azimuthMin
  azimuth_max <- azimuthMax
  if (is.null(azimuth_min) | is.null(azimuth_max)){
    print(paste(Sys.time(), " - ", "determining azimuth range with step: ", azimuthStep, sep=""))
    # figure out min/max azimuth
    latlon <- as.data.frame(
      sp::spTransform(
        raster::xyFromCell(dem_at_res,c(1),spatial=TRUE),
        sp::CRS("+proj=longlat")
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
    azimuth_min <- ceiling(azimuth_min / azimuthStep) * azimuthStep
    azimuth_max <- floor(azimuth_max / azimuthStep) * azimuthStep
    print(paste(
      Sys.time(),
      ' - ',
      'using calculated azimuth range: ',
      azimuth_min, ':', azimuth_max, ' (step: ', azimuthStep, ')',
      sep=""
    ))
  } else {
    print(paste(
      Sys.time(),
      ' - ',
      'using user-defined azimuth range: ',
      azimuth_min, ':', azimuth_max, ' (step: ', azimuthStep, ')',
      sep=""
    ))
  }

  print(paste(Sys.time(), ' - ', 'Calculating altitudes...', sep=""))

  # get altitude raster layers for each azimuth
  result = calculateMinAltitudeDistances(
    dem_at_res,
    azimuth_min,
    azimuth_max,
    settings = list(
      azimuth_step = azimuthStep,
      resolution_dem_target = targetResolution,
      grid_convergence = gridConvergence,
      correct_curvature = correctCurvature
    )
  )
  print(paste(Sys.time(), ' - ', 'Done calculating altitudes. Writing output files to: ', outDir, sep=""))
  for (azimuth in names(result)) {
    outFilename = paste(
      "altitude-distances_azimuth-", azimuth,
      "_dem-", tools::file_path_sans_ext(dem),
      "_resolution-", targetResolution,
      "_curv-", correctCurvature,
      print(as.numeric(Sys.time())*1000, digits=12),
      ".tif",
      sep=""
    )
    print(outFilename)
    # print(result[[azimuth]])
    r <- raster::raster(
      nrows = raster::nrow(dem_at_res),
      ncols = raster::ncol(dem_at_res),
      ext = raster::extent(dem_at_res),
      res = raster::res(dem_at_res),
      crs = raster::crs(dem_at_res),
      vals = result[[azimuth]]
    )
    raster::writeRaster(
      r,
      filename=paste(outDir, outFilename, sep=""),
      format="GTiff",
      overwrite=TRUE
    )
  }

  # return(result)
  # print(horizons_raster)
}
