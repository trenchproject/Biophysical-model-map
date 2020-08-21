library("ncdf4")
library("sf")
library("tidyr")
library("raster")

# Get average monthly temperatures from Downscaled CMIP3 and CMIP5 Climate and Hydrology Projections 
# (https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Welcome)

# NLDAS across USA
# BCSD-CMIP5-Climate-monthly, 1/8 degree BCSD projections, Ave Surface Air Temperature
# ccsm4, emission paths: RCP2.6, 6.0, 8.5
# No analysis, NetCDF


# temperature (longitude, latitude, months, projections)
# longitude: 1 - 460
#            235.4375 - 292.8125
# equivalent to -124.5625 - -67.1875

# latitude: 1 - 194
#           25.3125 - 49.4375

# months: 1 - 12

# projections: 1 - 160 (look into Projections5.txt file to see which number correspond to what)
#              ccsm4.5.rcp26 = 21  
#              ccsm4.5.rcp60 = 26  
#              ccsm4.5.rcp85 = 31  


# Read these first

# Download US boundary from https://www.igismap.com/united-states-shapefile-download-free-map-boundary-states-and-county/
shape <- st_read("Igismap/UnitedStates_Boundary.shp")

monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


AOI = aoi_get(state = "conus")
p = getGridMET(AOI, param = c('tmax', 'tmin', 'wind_vel'), startDate = Sys.Date() - 2)

r = raster::brick(p)


#______________________________________________________________________________________
# Create rasters from _tas.nc raw data sets. 

saveRaster <- function(year) {
  tas <- nc_open(paste0(year, "_tas.nc"))
  vals <- ncvar_get(tas)
  
  for(scenario in c(26, 60, 85)) {  # Correspond to ccsm4.5rcp2.6, 6.0 and 8.5
    for (month in c(1:12)) {
      if (scenario == 26) {
        projection = 21  # Check if these need to be modified. it depends on the year and the projections you select.
      } else if (scenario == 60) {
        projection = 26 
      } else {
        projection = 31
      }
      df <- vals[, , month, projection] %>% as.data.frame()
      colnames(df) <- seq(25.3125, 49.4375, by = 0.125)
      df$lon <- seq(-124.5625, -67.1875, by = 0.125)
      
      long <- gather(df, lat, temp, `25.3125`:`49.4375`, factor_key = TRUE)
      long[,2] <- as.numeric(as.character(long[,2]))
      
      coordinates(long) <- ~ lon + lat
      gridded(long) <- TRUE
      crs(long) <- "+proj=longlat +ellps=WGS84 +no_defs"
      raster <- raster(long)
      masked <- mask(raster, shape) %>% resample(r)
      filename <- paste0("year", year, "/rcp", scenario, "/", monthNames[month])
      writeRaster(masked, filename, overwrite = TRUE)
    }
  }
}

saveRaster(2070)


#_________________________________________________________________________________________
# Create dif files which are the temperature difference between 2020.
takeDif <- function(year) {
  for (scenario in c(26, 60, 85)) {
    dir.create(paste0("year", year, "dif"))
    for (month in c(1:12)) {
      dif <- raster(paste0("year", year, "/rcp", scenario, "/", monthNames[month], ".grd")) - raster(paste0("year2020/rcp", scenario, "/", monthNames[month], ".grd"))
      dirname <- paste0("year", year, "dif/rcp", scenario)
      dir.create(dirname)
      writeRaster(dif, filename = paste0(dirname, "/", monthNames[month]), overwrite = TRUE)
    }
  }
}


#____________________________________________________________________________________
# Full microclim data

for(month in 1:12) {
  filename <- paste0("air_temperature_degC_120cm/TA120cm_", month, ".nc")
  airbrick <- brick(filename)
  Ta <- mask(airbrick, shape) %>% resample(r)
  writeRaster(Ta, paste0("microclim/", monthNames[month]))
  
}

air <- nc_open("air_temperature_degC_120cm/TA120cm_1.nc")
airbrick <- brick("air_temperature_degC_120cm/TA120cm_1.nc")
Ta <- mask(airbrick, shape) %>% resample(r)
writeRaster(Ta, "microclim/Jan")

brick("microclim/Jan.grd")

#___________________________________________________________________________________
# For demo. Includes just 1pm.
for (month in 1:12) {
  filename <- paste0("microclim/", monthNames[month], ".grd")
  Ta <- brick(filename)
  airTemp <- Ta[[13]]
  writeRaster(airTemp, paste0("microclim_short/", monthNames[month]))
}

