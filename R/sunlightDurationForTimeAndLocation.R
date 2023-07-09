#'@title Calculate shades for a dem and a given time (UTC)
#'
#'@description Determines shades for each cell based on pre-calculated altitude raster files for a given time
#'
#'@useDynLib sunlightRCPP, .registration = TRUE
#'@importFrom Rcpp evalCpp
#'@import suncalc
#'@import raster
#'@import sp
#'@export

sunlightDurationForTimeAndLocation = function(
    timeStartUTC = '2022-08-20 0:00:00',
    timeStopUTC = '2022-08-20 23:59:00',
    timestep = 15, # minutes,
    lat = 42.75,
    lon = 0.85,
    dem = "zi_dtm_625_complete_cutlim.tif",
    demDir = "~/projects/INRAE/data/",
    altitudesDir = "~/projects/INRAE/data/altitudes/625m/",
    azimuthStep = 5,
    targetResolution = 12.5,
    cutVertically = TRUE,
    stripeWidth = 10000
) {
  # 1. figure out sun position based on input raster
  # load raster
  demFileAndPath = paste(demDir, dem, sep="")
  # print(paste(Sys.time(), " - ", "loading dem: ", dem, " from: ", demFileAndPath, sep=""))
  dem_original <- raster::raster(demFileAndPath)
  print(paste(Sys.time(), " - ", "loading dem: ", dem, " from: ", demFileAndPath, sep=""))
  dem_original <- raster::raster(demFileAndPath)
  spatial_point <- SpatialPoints(data.frame(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
  reprojected_point <- spTransform(spatial_point, raster::crs(dem_original))

  sunlightDuration <- 0

  dateStart = as.Date(timeStartUTC)
  dateCurrent <- dateStart;
  datetimeStart <- as.POSIXct(timeStartUTC ,  tz = "UTC");
  datetimeStop <- as.POSIXct(timeStopUTC,  tz = "UTC");
  dateTimeCurrent <- datetimeStart;
  sunPositionCurrent <- suncalc::getSunlightTimes(date = as.Date(timeStartUTC), lat = lat, lon = lon, keep = c( "sunrise", "sunset"))
  sunriseCurrent <- sunPositionCurrent$sunrise
  sunsetCurrent <- sunPositionCurrent$sunset
  hasSunCurrent <- FALSE

  while (dateTimeCurrent < datetimeStop) {
    if (as.Date(dateTimeCurrent) != dateCurrent){
      dateCurrent <- as.Date(dateTimeCurrent)
      sunPositionCurrent <- suncalc::getSunlightTimes(date = dateCurrent, lat = lat, lon = lon, keep = c( "sunrise", "sunset"))
      sunriseCurrent <- sunPositionCurrent$sunrise
      sunsetCurrent <- sunPositionCurrent$sunset
    }
    # print(paste('dateTimeCurrent ', dateTimeCurrent))
    # print(paste('sunriseCurrent ', sunriseCurrent))
    # print(paste('sunsetCurrent ', sunsetCurrent))
    if (dateTimeCurrent >= sunriseCurrent && dateTimeCurrent <= sunsetCurrent) {
      hasShade = shadeForTimeAndLocation(
        timeUTC = format(dateTimeCurrent),
        lat = lat,
        lon = lon,
        rasterDEM = dem_original,
        altitudesDir = "~/projects/INRAE/data/altitudes/625m/",
        azimuthStep = 5,
        targetResolution = 12.5,
        cutVertically = TRUE,
        stripeWidth = 10000
      )
      hasSunCurrent = !hasShade
      if (hasSunCurrent) {
        sunlightDuration = sunlightDuration + timestep
      }
      # print(paste('dateTimeCurrent ', dateTimeCurrent))
      # print(paste('hasSunCurrent ', hasSunCurrent))
    }
    dateTimeCurrent = dateTimeCurrent + timestep * 60
  }
  print(Sys.time())

  return(paste('hh:mm ', sprintf("%.02d:%.02d", sunlightDuration %/% 60, round(sunlightDuration %% 60))))
}
