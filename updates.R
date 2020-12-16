# Automatically updates data source every day at 2 am EST

setwd("/srv/shinyapps/RShiny_BiophysicalModelMap")

library("stringr")
library("raster")

print(str_c("----------------Starting update at ", Sys.time(), " UTC-----------------"))

valid_url <- function(url_in, t = 2){
  if(is.na(url_in)) {
    return (FALSE)
  } else {
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con, open="rt", timeout=t), silent = T)[1])
    suppressWarnings(try(close.connection(con), silent = T))
    ifelse(is.null(check), TRUE, FALSE)
  }
}

# Make it the shape of US
library(AOI)
library(climateR)
AOI = aoi_get(state = "conus")
p = getGridMET(AOI, param = 'tmax', startDate = Sys.Date() - 3)

r = raster::brick(p)

# Download US boundary from https://www.igismap.com/united-states-shapefile-download-free-map-boundary-states-and-county/
library("sf")
#shape <- st_read("UnitedStates_Boundary.shp")
shape <- st_read("cb_2019_us_nation_5m.shp")

# Remove all the files in the folder
do.call(file.remove, list(list.files("Forecasts", full.names = TRUE)))


#__________________________________________________________________________

x = 1
url1 = NA

while(!valid_url(url1)) {
  x = x + 1
  year = str_split(Sys.Date() - x, pattern = "-")[[1]][1]
  month = str_split(Sys.Date() - x, pattern = "-")[[1]][2]
  day = str_split(Sys.Date() - x, pattern = "-")[[1]][3]
  url1 = paste0("https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-9-month-forecast/time-series/", 
                 year, "/",
                 year, month, "/", 
                 year, month, day, "/")
}

url2 = NA
hour = 24
while(!valid_url(url2)) {
  hour = hour - 6
  url2 = paste0("https://www.ncei.noaa.gov/data/climate-forecast-system/access/operational-9-month-forecast/time-series/", 
                 year, "/",
                 year, month, "/", 
                 year, month, day, "/",
                 year, month, day, hour, "/tmp2m.01.", 
                 year, month, day, hour, ".daily.grb2")
}

filename <- "forecast.grb2"

print(paste0("------------------Getting data from ", url2, "---------------------"))

download.file(url = url2, destfile = filename, mode = "wb")

brick <- raster::brick(filename)

print("------------------Creating 28 layers--------------------")

for (n in 1:28) {
  layer <- brick[[(x + 1) * 4 - hour / 6 + n]] %>% 
    rotate() %>% 
    projectRaster(crs = "+proj=longlat +ellps=WGS84 +no_defs") %>%
    resample(r) %>% 
    mask(shape)
  filename <- paste0("Forecasts/tmp", str_replace_all(Sys.Date() + as.integer((n - 1) / 4), pattern = "-", replacement = ""), "_", ((n - 1) * 6) %% 24)
  writeRaster(layer, filename, overwrite = TRUE)
  
}


write.csv(data.frame("LastDate" = as.Date(paste0(year, "-", month, "-", day)), "x" = x, "Hour" = hour), "LastDate")

print(str_c("------------------Updates completed at ", Sys.time(), "UTC--------------------"))
