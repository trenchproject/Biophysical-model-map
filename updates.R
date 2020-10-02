# Supplemental file to update forecasts every week
# Data source (https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-9-month-forecast/time-series/)

#____________________________________________________________________________________
# Process forecast data (takes a bit to run)
updateData <- function() {
  lastDate <- read.csv("LastDate")

  x = 1
  url1 <- NA
  
  while(!valid_url(url1)) {
    x <- x + 1
    year <- str_split(Sys.Date() - x, pattern = "-")[[1]][1]
    month <- str_split(Sys.Date() - x, pattern = "-")[[1]][2]
    day <- str_split(Sys.Date() - x, pattern = "-")[[1]][3]
    url1 <- paste0("https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-9-month-forecast/time-series/", 
                  year, "/",
                  year, month, "/", 
                  year, month, day, "/")
  }
  
  
  url2 <- NA
  hour <- 24
  while(!valid_url(url2)) {
    hour <- hour - 6
    url2 <- paste0("https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-9-month-forecast/time-series/", 
                  year, "/",
                  year, month, "/", 
                  year, month, day, "/",
                  year, month, day, hour, "/tmp2m.01.", 
                  year, month, day, hour, ".daily.grb2")
  }
  
  # if(file.exists("forecast.grb2")) {
  #   file.remove("forecast.grb2")
  # }
  
  filename <- "forecast.grb2"
  
  download.file(url = url2, destfile = filename, mode = "wb")
  
  brick <- brick(filename)
  
  # a week from Sys.date() at noon
  
  oneWeek <- brick[[(7 + x) * 4 + (12 - hour) / 6 + 1]]
  
  oneMonth <- brick[[(30 + x) * 4 + (12 - hour) / 6 + 1]]
  
  
  oneWeek <- rotate(oneWeek) %>% projectRaster(crs = "+proj=longlat +ellps=WGS84 +no_defs")
  
  oneMonth <- rotate(oneMonth) %>% projectRaster(crs = "+proj=longlat +ellps=WGS84 +no_defs")
  
  # Make it the shape of US
  # library(AOI)
  # library(climateR)
  # AOI = aoi_get(state = "conus")
  # p = getGridMET(AOI, param = 'tmax', startDate = Sys.Date() - 3)
  # 
  # r = raster::brick(p)
  
  # Download US boundary from https://www.igismap.com/united-states-shapefile-download-free-map-boundary-states-and-county/
  # library("sf")
  shape <- st_read("UnitedStates_Boundary.shp")
  
  maskedWeek <- resample(oneWeek, r) %>% mask(shape)
  
  maskedMonth <- resample(oneMonth, r) %>% mask(shape)
  
  writeRaster(maskedWeek, "OneWeekForecast", overwrite = TRUE)
  
  writeRaster(maskedMonth, "OneMonthForecast", overwrite = TRUE)
  
  write.csv(data.frame("LastDate" = as.Date(paste0(year, "-", month, "-", day)), "x" = x, "Hour" = hour), "LastDate")

}
