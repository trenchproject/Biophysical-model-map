source("functions.R", local = TRUE)

monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

AOI = aoi_get(state = "conus")
x = 3

p = getGridMET(AOI, param = c('tmax', 'tmin', 'wind_vel'), startDate = Sys.Date() - x, endDate = Sys.Date() - x)
r = raster::brick(p)
names(r) = c('tmin', 'tmax', 'wind')
df <- rasterToPoints(r) %>% as.data.frame() %>% dplyr::select("x", "y")

elevData <- raster("elevData.grd") %>% resample(r)

updates <- read.csv("LastDate")
lastDate <- as.Date(updates$LastDate)


# Set up server
shinyServer <- function(input, output, session) {

  #___________________________________________________________________________________
  # renderUI's

  
  # Show options for dates and hours when "This week" is selected, and show options for scenarios when future years are selected.
  output$futureUI <- renderUI({
    validate(
      need(input$year, "")
    )
    if (input$year == "This week") {
      fluidRow(
        column(2, dateInput("date", "Specify date and time", value = Sys.Date(), min = Sys.Date(), max = Sys.Date() + 6)),
        column(2, style = "margin-top: 5px;",
               pickerInput("time", "", choices = c("0:00", "6:00", "12:00", "18:00"), 
                           options = list(style = "btn-success")))
      )
    } else if (input$year %in% c(2050, 2070, 2090)) {
      fluidRow(
        column(5, tipify(radioGroupButtons("scenario", "Scenarios", c("Optimistic", "Intermediate", "Pessimistic"), status = "danger", size = "sm", justified = TRUE), 
                         "RCP2.6 / RCP6.0 / RCP8.5"))
      )
    }
  })
  
  
  # Show choices for an hour to map if "Recent" or future years are selected.
  output$hourUI <- renderUI({
    validate(
      need(input$year, "")
    )
    if (input$year != "This week") {
      selectInput("hour", list(icon("glyphicon glyphicon-time", lib = "glyphicon"), "Hour"), choices = hours, selected = "01 PM")
    } 
  })
  

  # Show appropriate widgets depending on the species selected
  output$dynamicUI <- renderUI({
    
    weather <- tipify(radioGroupButtons("weather", list(icon("wind"), "Weather"), 
                           choices =  c('<i class="fas fa-sun"></i>' = "Clear", 
                                        '<i class="fas fa-cloud-sun"></i>' = "Partly sunny", 
                                        '<i class="fas fa-cloud"></i>' =  "Cloudy"), 
                           status = "info", justified = TRUE),
                      "Clear / Partly sunny / Cloudy")
    
    soil <- tipify(radioGroupButtons("soil", list(icon("seedling"), "Soil surface"),
                        choices = c('<i class="fab fa-pagelines"></i>' = "Grass", 
                                    '<i class="fas fa-circle"></i>' = "Dark soil", 
                                    '<i class="far fa-snowflake"></i>' = "Snow"),
                        status = "danger", justified = TRUE),
                   "Grass / Dark soil / Snow")
    
    if (input$species == "Lizard") {
      list(
        h4(div(tags$img(src = "Lizard_icon.png", height = 25), "Lizard")),
        tipify(numericInput("svl", list(icon("ruler"),"Snout-vent length (mm)"), value = 60), 
               "Length from the tip of the animal's nose to the opening of the cloaca at the tail base"),
        numericInput("mass", list(icon("weight"),"Mass (g)"), value = 10),
        tipify(selectInput("shade", list(icon("umbrella-beach"), "Shade"), choices = c("Exposed", "Covered")), 
               "Are the lizards exposed to the sun or in the shade?", placement = "top"),
        soil,
        tipify(radioGroupButtons("loc", list(icon("map-pin"), "Location"), 
                           choices = c('<i class="fas fa-seedling"></i>' = "Ground", 
                                       '<i class="fas fa-tree"></i>' = "Tree"),
                           status = "primary", justified = TRUE),
               "Lizard on ground / on trees")
      )
    } else if (input$species == "Grasshopper") {
      list(
        h4(div(tags$img(src = "Grasshopper_icon.png", height = 25), "Grasshopper")),
        numericInput("length", list(icon("ruler"), ("Length (mm)")), value = 50),
        tipify(sliderInput("abs_hopper", list(icon("tint"), "Absorptivity"), min = 0, max = 1, value = 0.6, step = 0.1), 
               "Grasshopper absorptivity to solar radiation"),
        soil,
        weather
      )
    } else if (input$species == "Salamander") {
      list(
        h4(div(tags$img(src="Salamander_icon.png", height = 20), "Salamander")),
        numericInput("diam", list(icon("ruler"), "Body diameter (mm)"), value = 10),
        weather
      )
    } else if (input$species == "Butterfly") {
      list(
        h4(div(tags$img(src = "Butterfly_icon.png", height = 20), "Butterfly")),
        numericInput("diam", list(icon("ruler-vertical"), "Thoracic diameter (mm)"), value = 3.6),
        numericInput("fur", list(icon("ruler-horizontal"), "Fur thickness (mm)"), value = 1.46),
        tipify(sliderInput("abs_butter", list(icon("tint"), "Absorptivity"), min = 0, max = 1, value = 0.6, step = 0.1), 
               "Wing solar absorbtivity"),
        tipify(selectInput("shade", list(icon("umbrella-beach"), "Shade"), choices = c("Exposed", "Covered")), 
               "Are the butterflies exposed to the sun or in the shade?", placement = "top"),
        soil,
        weather
      )
    } else if (input$species == "Snail") {
      list(
        h4(div(tags$img(src = "Snail_icon.png", height = 25), "Snail")),
        weather,
        numericInput("length", list(icon("ruler"), "Length (mm)"), value = 12)
      )
    } else if (input$species == "Mussel") {
      list(
        h4(div(tags$img(src = "Mussel_icon.png", height = 25), "Mussel")),
        numericInput("length", list(icon("ruler-horizontal"), "Length (cm)"), value = 10),
        numericInput("height", list(icon("ruler-vertical"), "Length (cm)"), value = 5),
        awesomeCheckbox("gape", "Are the mussels gaping?"),
        selectInput("group", "Group", choices = c("Aggregated", "Solitary")),
        weather
      )
    }
  })

  
  # Show option to color map where the temperature is above CTmax when the scale is discrete.
  output$tmaxUI <- renderUI({
    if (input$scale) { # input$scale == "Discrete"
      list(
        numericInput("CTmax", list(icon("thermometer-half"), "Critical thermal maximum (°C)"), value = 40),
        materialSwitch("red", status = "danger", label = "Show areas above CTmax in red")
      )
    }
  })
  
  
  #_______________________________________________________________________________________
  # reactives (hour, zenith, albedo, airTemp, bodyTemp)
  

  hour <- reactive({
    validate(
      need(input$year, "")
    )
    if (input$year == "This week") {  # input$time exist / input$hour doesn't exist
      hour <- as.numeric(str_split(input$time, ":00")[[1]][1])
    } else {  # input$hour exists
      validate(
        need(input$hour, "")
      )
      if (input$hour == "12 AM") {
        hour <- 0
      } else {
        hour <- as.numeric(strsplit(input$hour, " ")[[1]][1])
        if (strsplit(input$hour, " ")[[1]][2] == "PM" & hour != 12) {
          hour <- hour + 12
        } 
      }
    }
    hour
  })
  
  
  zenith <- reactive({
    validate(
      need(input$year, ""),
      need(input$species %in% c("Lizard", "Grasshopper", "Butterfly", "Mussel"), "")
    )
    
    df$zenith <- zenith_angle(doy = day_of_year(Sys.Date() - x), lat = df$y, lon = df$x, hour = hour())
    df_raster <- df
    coordinates(df_raster) <- ~ x + y
    gridded(df_raster) <- TRUE
    crs(df_raster) <- "+proj=longlat +ellps=WGS84 +no_defs"
    zenith <- raster(df_raster)
    
    zenith
  })
  
  
  albedo <- reactive({
    validate(
      need(input$soil, "")
    )
    if (input$soil == "Grass") {
      albedo = 0.25 
    } else if (input$soil == "Dark soil") {
      albedo = 0.1
    } else {
      albedo = 0.75
    }
  })
  
  
  airTemp <- reactive({
    validate(
      need(input$year, "")
    )
    
    month <- monthNames[as.numeric(strsplit(x = as.character(Sys.Date() - x), split = "-")[[1]][2])]

    if (input$year == "This week") {
      filename <- paste0("Forecasts/tmp", str_replace_all(input$date, "-", ""), "_", str_split(input$time, ":00")[[1]][1])
      airTemp <- raster(filename) %>% resample(r)
    } else {
      airTemp <- diurnal_temp_variation_sine(r$tmax - 273.15, r$tmin - 273.15, hour())
      
      if (input$year %in% c(2050, 2070, 2090)) {
        validate(need(input$scenario, ""))
        
        if (input$scenario == "Optimistic") {
          scn <- 26
        } else if (input$scenario == "Intermediate") {
          scn <- 60
        } else {
          scn <- 85
        }
        airTemp <- airTemp + raster(paste0("year", input$year, "dif/rcp", scn, "/", month, ".grd")) %>% resample(r)
      }
    }

    airTemp
  })
  
  bodyTemp <- reactive({
    validate(
      need(airTemp(), "")
    )

    if (input$species %in% c("Grasshopper", "Butterfly", "Salamander", "Snail", "Mussel")) {
      validate(
        need(input$weather, "")
      )
      if (input$weather == "Clear") {
        rad <- 900
        CC <- 0
        k_d <- 0.1  # proportion of solar radiation that is diffuse
        kt <- 0.75  # clearness index: https://www.homerenergy.com/products/pro/docs/latest/clearness_index.html
      } else if (input$weather == "Partly sunny") {
        rad <- 500
        CC <- 0.5
        k_d <- 0.5
        kt <- 0.5
      } else {
        rad <- 200
        CC <- 1
        k_d <- 1
        kt <- 0.25
      }
    }
    
    if (input$species == "Lizard") {
      validate(
        need(input$soil, "")
      )
      
      sun <- ifelse(input$shade == "Exposed", TRUE, FALSE)
      
      surface <- ifelse(input$loc == "Ground", TRUE, FALSE)

      Tb <- Tb_lizard(T_a = airTemp(),
                      T_g = airTemp() + 5,
                      u = r$wind, 
                      svl = input$svl, 
                      m = input$mass, 
                      psi = zenith(),
                      rho_S = albedo(), 
                      elev = elevData, 
                      doy = day_of_year(Sys.Date() - x), 
                      sun = sun, 
                      surface = surface,
                      alpha_S = 0.9,
                      alpha_L = 0.965,
                      F_d=0.5, 
                      F_r=0.5, 
                      F_a=0.5, 
                      F_g=0.5
                      )
      
    } else if (input$species == "Grasshopper") {
      
      Tb <- Tb_grasshopper(T_a = airTemp(), 
                           T_g = airTemp() + 5, 
                           u = r$wind, 
                           H = rad, 
                           K_t = kt, 
                           psi = zenith(), 
                           L = input$length / 1000, # in m 
                           Acondfact = 0.25, 
                           z = 0.001, 
                           abs = input$abs_hopper, 
                           r_g = albedo()
                           )

    } else if (input$species == "Salamander") {
      
      Tb <- Tb_salamander_humid(r_i = 4,
                                r_b = 1,
                                D = input$diam / 1000,
                                T_a = airTemp(),
                                elev = elevData,
                                e_s = 2.5,
                                e_a = 2,
                                Qabs = rad,
                                epsilon = 0.96
                                )
      
    } else if (input$species == "Butterfly") {
      
      shade <- ifelse(input$shade == "Exposed", FALSE, E)
      validate(
        need(input$diam, "")
      )
      
      Tb <- Tb_butterfly(T_a = airTemp(),
                         Tg = airTemp() + 5,
                         Tg_sh = airTemp() - 5, 
                         u = r$wind, 
                         H_sdir = rad,
                         H_sdif = rad / 2,
                         z = zenith(),
                         D = input$diam / 10, # cm
                         delta = input$fur,
                         alpha = input$abs_butter,
                         r_g = albedo(),
                         shade = shade
                         )
      
    } else if (input$species == "Snail") {
      
      Tb <- Tb_snail(temp = airTemp(), 
                     Len = 12 / 1000, # in m
                     solar = rad,
                     WS = r$wind,
                     CC = CC,
                     WL = 0,
                     WSH = 1
                     )
      
    } else if (input$species == "Mussel") {
      
      group <- ifelse(input$group == "Aggregated", "aggregated", "solitary")
      
      Tb <- Tb_mussel(L = input$length / 100, # in m
                      H = input$height / 100, # in m
                      T_a = airTemp(),
                      T_g = airTemp() + 5,
                      S = rad,
                      k_d = k_d,
                      u = r$wind,
                      psi = zenith(),
                      evap = input$gape,
                      cl = CC,
                      group = group
                      )
    }
    Tb
  })

  
  #________________________________________________________________________________
  # renders
  
  # Main map
  output$mymap <- renderLeaflet({
    validate(
      need(bodyTemp(), ""),
      need(airTemp(), "")
    )
    min <- round(minValue(bodyTemp()))
    max <- round(maxValue(bodyTemp()))
    
    if (input$scale) {  # If input$scale == "Discrete"
      CTmax <- input$scale
      
      arr <- seq(-20, 70, by = 10) # Displayable temperature range is -20 ~ 70 °C
      
      # Finding the range of data
      i = 1
      while(arr[i] < min) {
          i = i + 1
      }
      j = length(arr)
      while(arr[j] > max) {
        j = j - 1
      }
      # Creating bins in a way that in includes every multiple of 10s between min and max
      bins <- unique(c(min, arr[i:j], max))  
      
      # Pre-specify color so that each temperature range is colored the same regardless of the scale
      cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#cab2d6','#fdbf6f','#ff7f00','#e31a1c', '#6a3d9a')
      pal_tb <- cols[(i - 1):j]
      
      # To color areas above CTmax in red
      if (input$red && CTmax < max) {  # No need to add red if Ctmax was above the maximum temperature on the map
        if (CTmax %% 10 != 0) {  # No bin needed to add if CTmax was a multiple of 10
          bins <- sort(c(bins, CTmax))
        }
        bins <- c(bins[1:(which(bins == CTmax))], max)
        pal_tb <- c(pal_tb[1:(which(bins == CTmax) - 1)], "red")
      }
      
      pal <- colorBin(palette = pal_tb,
                      domain = c(min, max),
                      bins = bins,
                      na.color = "transparent"
             )
  
      #___________________________________________________________________
      # Coloring for air temperature
      
      validate(
        need(airTemp(), "")
      )
      minTa <- round(minValue(airTemp()))
      maxTa <- round(maxValue(airTemp()))
      
      # Same process for air temperature. Finding the range.
      i = 1
      while(arr[i] < minTa) {
        i = i + 1
      }
      j = length(arr)
      while(arr[j] > maxTa) {
        j = j - 1
      }
      
      binsTa <- unique(c(minTa, arr[i:j], maxTa))
      palTa <- cols[(i - 1):j]
      pal_air <- colorBin(palette = palTa,
                          domain = c(minTa, maxTa),
                          bins = binsTa,
                          na.color = "transparent"
                          )
      
    } else {  # If input$scale == "Continuous"
      pal <- colorNumeric(palette = c('#ffff33', '#f781bf', '#999999', '#4daf4a', '#377eb8', '#984ea3', '#ff7f00', '#e41a1c', '#a65628'),
                          domain = c(-30, 70),
                          na.color = "transparent"
                          )
      pal_air <- pal
    }
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addRasterImage(x = bodyTemp(), colors = pal, group = "Body temperatures", opacity = 1) %>%
      addRasterImage(x = airTemp(), colors = pal_air, group = "Air temperatures", opacity = 1) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addLayersControl(baseGroups = c("Body temperatures", "Air temperatures")) %>%
      addLegend(pal = pal,
                opacity = 1,
                values = c(min, max),
                position = "bottomright",
                title = "Body temperatures (°C)")
  })
  
  
  # Title of the map
  output$title <- renderUI({
    validate(
      need(input$year, "Select time to map")
    )
    if (input$year == "Recent") {
      day <- paste("on", Sys.Date() - x)
    } else if (input$year == "This week") {
      day <- paste("on", input$date, "at", input$time)
    } else {
      day <- paste("in", monthNames[as.numeric(strsplit(x = as.character(Sys.Date() - x), split = "-")[[1]][2])], input$year)
    }
    HTML("<h4>", input$species, "body temperature", day, "</h4>")
  })

  
  # Map legend
  observeEvent(input$mymap_groups, {
    min <- round(minValue(bodyTemp()))
    max <- round(maxValue(bodyTemp()))
    
    minTa <- round(minValue(airTemp()))
    maxTa <- round(maxValue(airTemp()))
    
    if (input$scale) {  #If input$scale == "Discrete"
      CTmax <- input$CTmax
      
      arr <- seq(-20, 70, by = 10)
      i = 1
      while(arr[i] < min) {
        i = i + 1
      }
      j = length(arr)
      while(arr[j] > max) {
        j = j - 1
      }
      
      bins <- unique(c(min, arr[i:j], max))
      cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#cab2d6','#fdbf6f','#ff7f00','#e31a1c', '#6a3d9a')
  
      pal_tb <- cols[(i - 1):j]
      
      if (input$red && CTmax < max) {
        if (CTmax %% 10 != 0) {
          bins <- sort(c(bins, CTmax))
        }
        bins <- c(bins[1:(which(bins == CTmax))], max)
        pal_tb <- c(pal_tb[1:(which(bins == CTmax) - 1)], "red")
      }
      
      pal <- colorBin(palette = pal_tb,
                      domain = c(min, max),
                      bins = bins,
                      na.color = "transparent"
      )
      
      #____________________________________________________________________-
      # Coloring for air temperature
      
      i = 1
      while(arr[i] < minTa) {
        i = i + 1
      }
      j = length(arr)
      while(arr[j] > maxTa) {
        j = j - 1
      }
      
      binsTa <- unique(c(minTa, arr[i:j], maxTa))
      palTa <- cols[(i - 1):j]
      pal_air <- colorBin(palette = palTa,
                          c(minTa, maxTa),
                          bins = binsTa,
                          na.color = "transparent"
      )
    } else {  # If input$scale == "Continuous"
      pal <- colorNumeric(palette = c('#ffff33', '#f781bf', '#999999', '#4daf4a', '#377eb8', '#984ea3', '#ff7f00', '#e41a1c', '#a65628'),
                          domain = c(-30, 70),
                          na.color = "transparent"
                          )
      pal_air <- pal
    }

    if (input$mymap_groups == "Body temperatures") {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = pal, 
                  opacity = 1,
                  values = c(min, max),
                  group = "Body legend",
                  position = "bottomright",
                  title = "Body temperatures (°C)")
    } else {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = pal_air,
                  opacity = 1,
                  values = c(minTa, maxTa),
                  group = "Air legend",
                  position = "bottomright",
                  title = "Air temperatures (°C)")
    }
  })
}

