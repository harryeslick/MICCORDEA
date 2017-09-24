################################################################################
# title         : 00 - run_epirice_simulations.R;
# purpose       : script to run EPIRICE model;
# producer      : prepared by J.Aunario & A. Sparks;
# last update   : in Toowoomba, Qld, Australia, Sept 2017;
# inputs        : raster file of planting date of area of interest
#                 weather data in a raster format;
# outputs       : maps of AUDPC for rice blast and bacterial blight;
# comments      : this script accesses a weather database at IRRI;
################################################################################

# Load R libraries
library(cropsim) # this is the package that has EPIRICE in it
library(RODBC) # this is the package that connects to the database at IRRI


# as an example, this selection is for East South Africa, aoi = area of interest
aoi <-
    extent(28.90833, 40.40828, -11.65833, 4.174937)

# raster file that has planting date for rice
plantingdate <-
  raster("")

# set the projection of the planting date raster to +proj=longlat +datum=WGS84"
projection(plantingdate) <-
  projection(raster())

# set any values of 0 in planting data raster to NA
plantingdate[plantingdate[] == 0] <-
  NA

# conversion for date
plantingdate <- plantingdate * 14 - 7

# set the extent of the planting date using,
# creates the area of interest
ch <-
  cellsFromExtent(plantingdate, aoi)

plantingdate[ch] <- doyFromDate(as.Date("2050-6-1"))

# where output files will be written to disk
outpath <- ""

# this function simplifies running the model for both leaf blast and bacterial leaf blight
myfun <- function(...) {
  blast <- try(leafBlast(..., wetness = 0, onset = 15), silent = TRUE)
  if (class(blast) != "try-error") {
    blstout <- c(blast@d$severity, sum(blast@d$severity, na.rm = TRUE))
  } else {
    blstout <- rep(-9999, 122)
  }
  names(blstout) <-
    paste("blast", c(sprintf("%03d", 0:120), "audpc"), sep = "_")

  bblight <-
    try(bactBlight(..., wetness = 0, onset = 20), silent = TRUE)
  if (class(bblight) != "try-error") {
    blhtout <-
      c(bblight@d$severity, sum(bblight@d$severity, na.rm = TRUE))
  } else {
    blhtout <- rep(-9999, 122)
  }
  names(blhtout) <-
    paste("bblight", c(sprintf("%03d", 0:120), "audpc"), sep = "_")

  return(c(blstout, blhtout))
}

# the yy object is the output from the EPIRICE model
yy <- simulate_region(
  myfun,

  # use the planting date raster to determine the season start
  plantingdate = plantingdate,

  # this is database where the weather data is stored
  wthdsn = "",

  # this is the table in the database
  wthdataset = "afra2_5m",

  # there are more than one year in the database, this selects the proper one
  years = 2050,

  # aoi = area of interest, as set above
  region = aoi,

  # we see the messages in the R console
  verbose = TRUE,

  # where are we writing the files?
    outpath = outpath,

  format = "GTiff",
  # the files will be written as geotiff files, and compressed
  options = c("COMPRESS = LZW", "TFW = YES"),

  # if the output file exists and you re-run the model, you will get errors if this is not TRUE
  overwrite = TRUE
)

# plot the outputs from above
for (i in 1:length(yy)) {
  xx <- raster(yy[i])
  NAvalue(xx) <- -9999
  plot(xx)
  Sys.sleep(1)
}

#eos
