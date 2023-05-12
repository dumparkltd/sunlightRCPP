#'@title Calculate a shade raster for a dem and a given time (UTC)
#'
#'@import raster
#'@export
doShades <- function(altitude, altitudes) {
  print(paste("doShades ", altitude, raster::ncell(altitudes), sep=" - "))
  shades <- c()
  for (i in 1:raster::ncell(altitudes)) {
    if (altitude < altitudes[i]) {
      shades <- append(shades, 1)
    } else {
      shades <- append(shades, 0)
    }
  }
  print(shades)
  return(raster::raster(
    nrows = raster::nrow(altitudes),
    ncols = raster::ncol(altitudes),
    ext = raster::extent(altitudes),
    res = raster::res(altitudes),
    crs = raster::crs(altitudes),
    vals = shades
  ))
}
