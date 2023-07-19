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

shadeForTimeAndLocation = function(
    timeUTC = '2022-08-20 7:45:00',
    lat = 42.75,
    lon = 0.85,
    dem = "zi_dtm_625_complete_cutlim.tif",
    rasterDEM = NULL,
    demDir = "~/projects/INRAE/data/",
    altitudesDir = "~/projects/INRAE/data/altitudes/625m/",
    azimuthStep = 5,
    targetResolution = 12.5,
    cutVertically = TRUE,
    stripeWidth = 10000
) {
  # 1. figure out sun position based on input raster

  if (is.null(rasterDEM)) {
    # load raster
    demFileAndPath = paste(demDir, dem, sep="")
    # print(paste(Sys.time(), " - ", "loading dem: ", dem, " from: ", demFileAndPath, sep=""))
    dem_original <- raster::raster(demFileAndPath)
  } else {
    dem_original = rasterDEM
  }
  spatial_point <- SpatialPoints(data.frame(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
  reprojected_point <- spTransform(spatial_point, raster::crs(dem_original))
# print(reprojected_point)
  # figure out azimuth
  datetime <- as.POSIXct(timeUTC,  tz = "UTC");
  sun_position <- suncalc::getSunlightPosition(
    date= datetime,
    lat = lat,
    lon = lon,
    keep = c( "azimuth", "altitude")
  )

  azimuth <- round(rad2deg(sun_position$azimuth)+180)
  azimuth <- round(azimuth/azimuthStep)*azimuthStep
  altitude <- rad2deg(sun_position$altitude)
  print(paste('azimuth: ', azimuth, sep=''))
  print(paste('altitude: ', altitude, sep=''))
  # figure out stripe
  col = raster::colFromX(dem_original,xmin(reprojected_point))
  xres = raster::xres(dem_original) # in px
  stripe_w_px <- stripeWidth/xres # in p
  stripe_no <- ceiling(col/stripe_w_px)

  # read alt file for azimuth and stripe
  altFileAndPath = paste(altitudesDir, 'altitudes_azimuth-', azimuth, '_stripe-',stripe_no,'.tif', sep="")
  print(altFileAndPath)
  altitudes <- raster::raster(altFileAndPath)
  min_alt <- extract(altitudes, reprojected_point)
  print(paste('required altitude at location: ', min_alt, sep=''))
  has_shade <- altitude < min_alt
  print(paste('has shade ', has_shade, sep=""))
  return (has_shade)
}
