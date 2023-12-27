#'@useDynLib sunlightRCPP, .registration = TRUE
#'@importFrom Rcpp evalCpp
#'@import doParallel
#'@import foreach
#'@export

sunlightDurationForTimeAndStripes = function(
    timeStartUTC = '2022-08-20 0:00:00',
    timeStopUTC = '2022-08-20 23:59:00',
    timestep = 15, # minutes,
    altitudesDir, # = "~/projects/INRAE/data/altitudes/RCPP/stripes/",
    altitudeFilePattern, # = "altitudes_azimuth-{azi}_res-50_inc-1-03_stripe-{stripe}.tif",
    altitudeFilePlaceholder = 'azi',
    altitudeFilePlaceholderStripe = 'stripe',
    outDir, # = "~/projects/INRAE/data/altitudes/RCPP/stripes/duration/",
    azimuthStep = 2,
    azimuthMin = 56,
    stripeNo = 40
) {
  month = format(as.Date(timeStartUTC), "%m")
  day = format(as.Date(timeStartUTC), "%d")
  match <- paste("\\{", altitudeFilePlaceholderStripe, "\\}", sep="")
  pattern <- altitudeFilePattern

  cores = parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  foreach (i = seq(1, stripeNo, by = 1)) %dopar% {

    altFile <- stringr::str_replace(pattern, match, as.character(i))
    outFilename = paste(
      "duration_",
      month,
      day,
      "_int-",
      timestep,
      "_stripe-",
      i,
      ".tif",
      sep=""
    )
    sunlightDurationForTimeAndArea (
      timeStartUTC = timeStartUTC,
      timeStopUTC = timeStopUTC,
      timestep = timestep,
      altitudesDir = altitudesDir,
      altitudeFilePattern = altFile,
      altitudeFilePlaceholder = 'azi',
      outDir = outDir,
      outFilename = outFilename,
      azimuthStep = azimuthStep,
      azimuthMin = azimuthMin
    )
  }
}
