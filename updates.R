# Supplemental file to update forecasts every week
# Data source (https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-9-month-forecast/time-series/)

#____________________________________________________________________________________
# Process forecast data (takes a bit to run)
updateData <- function() {
  lastDate <- read.csv("LastDate")
  
  if (lastDate$LastDate < Sys.Date() - 7 - lastDate$x + 1) {
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
    library(AOI)
    library(climateR)
    AOI = aoi_get(state = "conus")
    p = getGridMET(AOI, param = 'tmax', startDate = Sys.Date() - 3)
    
    r = raster::brick(p)
    
    # Download US boundary from https://www.igismap.com/united-states-shapefile-download-free-map-boundary-states-and-county/
    library("sf")
    shape <- st_read("UnitedStates_Boundary.shp")
    
    maskedWeek <- resample(oneWeek, r) %>% mask(shape)
    
    maskedMonth <- resample(oneMonth, r) %>% mask(shape)
    
    writeRaster(maskedWeek, "OneWeekForecast", overwrite = TRUE)
    
    writeRaster(maskedMonth, "OneMonthForecast", overwrite = TRUE)
    
    # 
    # library("ncdf4")
    # tempdata <- nc_open("tmp2m.01.2020091512.daily.nc")
    # 
    # 
    # # get the values as vectors
    # temp <- ncvar_get(tempdata, varid = "TMP_P0_L103_GGA0")
    # lon <- ncvar_get(tempdata, varid = "lon_0")
    # lat <- ncvar_get(tempdata, varid = "lat_0")
    # hours <- ncvar_get(tempdata, varid = "forecast_time0")
    # 
    # 
    # # create empty df
    # df <- data.frame("lon" = NA, "lat" = NA, "temp" = NA)
    # 
    # 
    # # Fill in the data frame
    # 
    # hour <- 6 # How many hours later? (multiple of 6)
    # 
    # n <- 0
    # for (ln in which(lon > 235.4375 & lon < 292.8125)) {
    #   for (lt in which(lat > 25.3125 & lat < 49.4375)) {
    #     n <- n + 1
    #     df <- rbind(df, c(lon[ln], lat[lt], temp[ln, lt, hour / 6]))
    #   }
    # }
    # df <- df %>% na.omit()
    # df$lon <- df$lon - 360
    # df$temp <- df$temp - 273.15
    # 
    # # make it a raster
    # coordinates(df) <- ~ lon + lat
    # crs(df) <- "+proj=longlat +ellps=WGS84 +no_defs"
    # pixels <- SpatialPixelsDataFrame(df, tolerance = 4.88297e-05, df@data)
    # raster <- raster(pixels)
    # 
    # # Make it the shape of US
    # library(AOI)
    # library(climateR)
    # AOI = aoi_get(state = "conus")
    # p = getGridMET(AOI, param = 'tmax', startDate = Sys.Date() - 2)
    # 
    # r = raster::brick(p)
    # 
    # # Download US boundary from https://www.igismap.com/united-states-shapefile-download-free-map-boundary-states-and-county/
    # library("sf")
    # shape <- st_read("UnitedStates_Boundary.shp")
    # masked <- mask(raster, shape) %>% resample(r)
    # 
    
  
    write.csv(data.frame("LastDate" = as.Date(paste0(year, "-", month, "-", day)), "x" = x, "Hour" = hour), "LastDate")
  
  }
}
