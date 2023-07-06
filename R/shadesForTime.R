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

shadesForTime = function(
    dem = "raster1.tif",
    demDir = "~/projects/INRAE/data/",
    timeUTC = '2022-08-20 9:45:00',
    altitudesDir = "~/projects/INRAE/data/altitudes/RCPP/",
    outDir = "~/projects/INRAE/data/shademaps/RCPP/",
    azimuthStep = 1,
    targetResolution = 10
) {
  # 1. figure out sun position based on input raster
  # load raster
  demFileAndPath = paste(demDir, dem, sep="")
  print(paste(Sys.time(), " - ", "loading dem: ", dem, " from: ", demFileAndPath, sep=""))
  dem_original = raster::raster(demFileAndPath)
  # figure out min/max azimuth
  latlon <- as.data.frame(
    sp::spTransform(
      raster::xyFromCell(dem_original,c(1),spatial=TRUE),
      sp::CRS("+proj=longlat")
    )
  )
  lon = latlon$x
  lat = latlon$y
  print(paste(lon, lat, sep=" "))
  datetime <- as.POSIXct(timeUTC,  tz = "UTC");
  sun_position <- suncalc::getSunlightPosition(
    date= datetime,
    lat = lat,
    lon = lon,
    keep = c( "azimuth", "altitude")
  )
  print(sun_position)
  azimuth = round(rad2deg(sun_position$azimuth)+180)
  altitude = rad2deg(sun_position$altitude)
  azimuth = round(azimuth / azimuthStep) * azimuthStep
  print(paste(Sys.time(), " - ", "assumed sun position for given time. Azimuth (rounded): ", azimuth, " Altitude (degrees): ", altitude, sep = ""))
  # 2. load corresponding altitudes file
  altFilename = paste(
    "azimuth-", azimuth,
    "_dem-", tools::file_path_sans_ext(dem),
    "_resolution-", targetResolution,
    ".tif",
    sep=""
  )

  altFile = paste(altitudesDir, altFilename, sep="")
  print(paste(Sys.time(), " - ", "loading altitude file (", altFilename, ")for given azimuth...", sep = ""))
  altitudes <- raster::raster(altFile)
  print(paste(Sys.time(), " - ", "calculating shades for altitude...", sep = ""))
  startTS = Sys.time()
  if (altitude > raster::maxValue(altitudes)) {
    shades <- raster::raster(
      nrows = raster::nrow(altitudes),
      ncols = raster::ncol(altitudes),
      ext = raster::extent(altitudes),
      res = raster::res(altitudes),
      crs = raster::crs(altitudes),
      vals = 0
    )
  } else {
    shadeMatrix <- get_shades_for_altitudes_cpp(
      raster::as.matrix(altitudes),
      altitude
    )
    shades <- raster::raster(
      nrows = raster::nrow(altitudes),
      ncols = raster::ncol(altitudes),
      ext = raster::extent(altitudes),
      res = raster::res(altitudes),
      crs = raster::crs(altitudes),
      vals = shadeMatrix
    )
  }
  endTS = Sys.time()
  outFilename = paste(
    "shade-",
    gsub("-", "", gsub(":", "", gsub(" ", "_", timeUTC))),
    "_azi-", azimuth,
    "_alt-", round(altitude),
    "_dem-", tools::file_path_sans_ext(dem),
    "_resolution-", targetResolution,
    "_", floor(as.numeric(endTS)),
    ".tif",
    sep=""
  )
  outFile = paste(outDir, outFilename, sep="")
  print(paste(Sys.time(), " - ", "shades calculated, writing raster file: ", outFile, sep = ""))
  print(paste("Shade calculation took", endTS - startTS, "seconds", sep = " "))
  raster::writeRaster(
    shades,
    filename=outFile,
    format="GTiff",
    overwrite=TRUE
  )
}
