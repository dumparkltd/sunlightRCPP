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

sunlightDurationForTimeAndArea = function(
    timeStartUTC = '2022-08-20 0:00:00',
    timeStopUTC = '2022-08-20 23:59:00',
    timestep = 15, # minutes,
    altitudesDir = "~/projects/INRAE/data/altitudes/RCPP/stripes/",
    altitudeFilePattern = "altitudes_azimuth-{azi}_res-50_inc-1-03_stripe-1.tif",
    altitudeFilePlaceholder = 'azi',
    outDir = "~/projects/INRAE/data/altitudes/RCPP/stripes/duration/",
    outFilename = "duration.tif",
    azimuthStep = 2,
    azimuthMin = 56
) {

  # figure out lat/lon to calculate sunlight times from any altitudes file
  # load raster
  altFile <- stringr::str_replace(altitudeFilePattern, '\\{azi\\}', as.character(azimuthMin))
  altFileAndPath <- paste(altitudesDir, altFile, sep="")
  print(paste(Sys.time(), " - ", "loading sample altitudes file: ", altFile, " from: ", altFileAndPath, "for any azimuth: ", azimuthMin , sep=""))

  sampleRaster = raster::raster(altFileAndPath)
  # print ('sampleRaster loaded')

  # prepare result matrix
  sunlightDuration <- raster::as.matrix(sampleRaster)
  sunlightDuration[] <- 0
  # print ('result matrix prepared')

  # figure out min/max azimuth
  latlon <- as.data.frame(
    sp::spTransform(
      raster::xyFromCell(sampleRaster,c(1),spatial=TRUE),
      sp::CRS("+proj=longlat")
    )
  )
  lon = latlon$x
  lat = latlon$y
  # print(paste('lat lon from sampleRaster: ', lat, '-', lon, sep=''))

  dateStart = as.Date(timeStartUTC)
  datetimeStart <- as.POSIXct(timeStartUTC,  tz = "UTC");
  datetimeStop <- as.POSIXct(timeStopUTC,  tz = "UTC");

  sunCurrent <- suncalc::getSunlightTimes(
    date = as.Date(datetimeStart),
    lat = lat,
    lon = lon,
    keep = c( "sunrise", "sunset")
  )
  sunriseCurrent <- sunCurrent$sunrise
  sunsetCurrent <- sunCurrent$sunset

  dateCurrent <- dateStart;
  datetimeCurrent <- datetimeStart;
  print(paste(Sys.time(), " - ", "calculating potential sunlight"))

  while (datetimeCurrent < datetimeStop) {
    if (as.Date(datetimeCurrent) != dateCurrent){
      dateCurrent <- as.Date(datetimeCurrent)
      sunCurrent <- suncalc::getSunlightTimes(
        date = dateCurrent,
        lat = lat,
        lon = lon,
        keep = c( "sunrise", "sunset")
      )
      sunriseCurrent <- sunCurrent$sunrise
      sunsetCurrent <- sunCurrent$sunset
    }

    if (datetimeCurrent >= sunriseCurrent && datetimeCurrent <= sunsetCurrent) {
      sunPositionCurrent <- suncalc::getSunlightPosition(
        date = datetimeCurrent,
        lat = lat,
        lon = lon,
        keep = c("azimuth", "altitude")
      )
      azimuth = round(rad2deg(sunPositionCurrent$azimuth)+180)
      altitude = rad2deg(sunPositionCurrent$altitude)
      azimuth = round(azimuth / azimuthStep) * azimuthStep
      # print(paste('alt: ', altitude, sep = ''))
      # print(paste('azi: ', azimuth, sep = ''))

      altFile <- stringr::str_replace(altitudeFilePattern, '\\{azi\\}', as.character(azimuth))
      altFileAndPath <- paste(altitudesDir, altFile, sep="")
      altitudesRaster = raster::raster(altFileAndPath)

      # print(paste(Sys.time(), " - ", "loading altitudes file: ", altFile, " from: ", altFileAndPath, " for azimuth: ", azimuth , sep=""))
      # print(paste('max alt: ', raster::maxValue(altitudesRaster), sep = ''))

      if (altitude > raster::maxValue(altitudesRaster)) {
        # print('all sunlight')
        sunlight <- raster::raster(
          nrows = raster::nrow(altitudesRaster),
          ncols = raster::ncol(altitudesRaster),
          ext = raster::extent(altitudesRaster),
          res = raster::res(altitudesRaster),
          crs = raster::crs(altitudesRaster),
          vals = 1
        )
        sunlightMatrix <- raster::as.matrix(sunlight)
      } else {
        sunlightMatrix <- get_sunlight_for_altitudes_cpp(
          raster::as.matrix(altitudesRaster),
          altitude
        )

      }
      sunlightDuration = sunlightDuration + (sunlightMatrix * timestep)
    }
    datetimeCurrent = datetimeCurrent + timestep * 60
  }
  print(paste(Sys.time(), " - ", "DONE calculating potential sunlight. Writing output file to:", paste(outDir, outFilename, sep="")))
  rasterSunlightDuration <- raster::raster(
    nrows = raster::nrow(sampleRaster),
    ncols = raster::ncol(sampleRaster),
    ext = raster::extent(sampleRaster),
    res = raster::res(sampleRaster),
    crs = raster::crs(sampleRaster),
    vals = sunlightDuration
  )
  raster::writeRaster(
    rasterSunlightDuration,
    filename=paste(outDir, outFilename, sep=""),
    format="GTiff",
    overwrite=TRUE
  )
}
