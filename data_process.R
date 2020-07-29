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

tas <- nc_open("Extraction_tas.nc")
tas50 <- nc_open("2050_tas.nc")
tas <- nc_open("2090_tas.nc")

tas50 <- ncvar_get(tas50)
temperature <- ncvar_get(tas)
temperature <- temperature[,,,c(21, 26, 31)]   # Get ccsm4.5rcp2.6, 6.0 and 8.5
sum(temperature[,,8,3] > temperature[,,8,1], na.rm = TRUE)
max(temperature[,,8,3], na.rm = TRUE)
max(temperature[,,8,1], na.rm = TRUE)

# temperature (longitude, latitude, months, projections)
# longitude: 1 - 460
#            235.4375 - 292.8125
# equivalent to -124.5625 - -67.1875

# latitude: 1 - 194
#           25.3125 - 49.4375

# months: 1 - 12

# projections: 1 - 3  (look into Projections5.txt file to see which number correspond to what)
#              ccsm4.5.rcp26 = 1  
#              ccsm4.5.rcp60 = 2  
#              ccsm4.5.rcp85 = 3  


# Read these first
shape <- st_read("Igismap/UnitedStates_Boundary.shp")

monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


AOI = aoi_get(state = "conus")
p = getGridMET(AOI, param = c('tmax', 'tmin', 'wind_vel'), startDate = Sys.Date() - 2)

r = raster::brick(p)


#______________________________________________________________________________________

saveRaster <- function(year) {
  tas <- nc_open(paste0(year, "_tas.nc"))
  vals <- ncvar_get(tas)
  
  for(scenario in c(26, 60, 85)) {  # Correspond to ccsm4.5rcp2.6, 6.0 and 8.5
    for (month in c(1:12)) {
      if (scenario == 26) {
        projection = 21
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

rcp26 <- raster("year2050/rcp26/Aug.grd")
rcp85 <- raster("year2050/rcp85/Aug.grd")


#_________________________________________________________________________________________

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

takeDif(2070)


#____________________________________________________________________________________

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

for (month in 2:12) {
  filename <- paste0("microclim/", monthNames[month], ".grd")
  Ta <- brick(filename)
  airTemp <- Ta[[13]]
  writeRaster(airTemp, paste0("microclim_short/", monthNames[month]))
}

airTemp

rasterVis::levelplot(rcp26)
rasterVis::levelplot(rcp85)
