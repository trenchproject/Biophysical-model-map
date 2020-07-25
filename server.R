source("functions.R", local = TRUE)
monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

AOI = aoi_get(state = "conus")

elevData <- raster("elevData.grd")


shinyServer <- function(input, output, session) {
  
  r <- reactive({
    month <- which(input$monthAll %in% monthNames)
    
    if (input$options == "gridMET") {
      # For gridMET, we need tmax, tmin and wind speed from the specified month, either from this year or last year, whichever that's closer.
      year = 2020
      if (which(input$monthAll %in% monthNames) > 7) {
        year = year - 1
      }
      
      date <- as.Date(paste0(year, "-", month, "-15"))
      #p = getGridMET(AOI, param = c('tmax', 'tmin', 'wind_vel'), startDate = "2020-6-15", endDate = "2020-6-15")
      
      p = getGridMET(AOI, param = c('tmax', 'tmin', 'wind_vel'), startDate = date, endDate = date)
      r = raster::brick(p)
      names(r) = c('tmin', 'tmax', 'wind')
      rm(p)
    } else {
      # For microclim, we just need the wind speed but from 1990
      
      date <- as.Date(paste0("1990-", month, "-15"))
      p = getGridMET(AOI, param = 'wind_vel', startDate = date, endDate = date)
      r = raster::brick(p)
      names(r) = "wind"
    }
    r
  })
  
  hour <- reactive({
    if (input$hour == "12 AM") {
      hour <- 0
    } else {
      hour <- as.numeric(strsplit(input$hour, " ")[[1]][1])
      if (strsplit(input$hour, " ")[[1]][2] == "PM" & hour != 12) {
        hour <- hour + 12
      } 
    }
    hour
  })
  
  output$future <- renderUI({
    if(input$year != 2020) {
      fluidRow(
        column(5, tipify(radioGroupButtons("scenario", "Scenarios", c("Optimistic", "Intermediate", "Pessimistic"), status = "danger", size = "sm", justified = TRUE),"RCP2.6 / RCP6.0 / RCP8.5"))
        # column(4, offset = 1, selectInput("month", "Month", choices = monthNames))
      )
    }
  })
  
  # widgets to show
  output$dynamicUI <- renderUI({
    weather <- tipify(radioGroupButtons("weather", list(icon("wind"), "Weather"), 
                           choices =  c('<i class="fas fa-sun"></i>' = "Clear", 
                                        '<i class="fas fa-cloud-sun"></i>' = "Partly sunny", 
                                        '<i class="fas fa-cloud"></i>' =  "Cloudy"), 
                           status = "info", justified = TRUE),
                      "Clear / Partly sunny / Cloudy")
    # weather <- selectInput("weather", list(icon("wind"), "Weather"), 
    #                        choices =  c("Sunny", "Partially cloudy", "Overcast"))
                           #choicesOpt = list(icon = c("fa fa-sun", "fa fa-cloud-sun", "glyphicon glyphicon-cloud")),
                           #options = list(style = "none"))
    
    soil <- tipify(radioGroupButtons("soil", list(icon("seedling"), "Soil surface"),
                        choices = c('<i class="fab fa-pagelines"></i>' = "Grass", 
                                    '<i class="fas fa-circle"></i>' = "Dark soil", 
                                    '<i class="far fa-snowflake"></i>' = "Snow"),
                        status = "danger", justified = TRUE),
                   "Grass / Dark soil / Snow")
    # soil <- selectInput("Soil surface", list(icon("seedling"), "Soil"),
    #             choices = c("Grass", "Dark soil", "Snow"))
                # choicesOpt = list(icon = c("fa fa-pagelines", "fa fa-circle", "fa fa-snowflake")),
                # options = list(style = "none"))
    if(input$species == "Lizard") {
      list(
        h4(div(tags$img(src="Lizard_icon.png", height = 25),"Lizard")),
        tipify(numericInput("svl", list(icon("ruler"),"Snout-vent length (mm)"), value = 60), "Length from the tip of the animal's nose to the opening of the cloaca at the tail base"),
        numericInput("mass", list(icon("weight"),"Mass (g)"), value = 10),
        tipify(selectInput("shade", list(icon("umbrella-beach"), "Shade"), choices = c("Exposed", "Covered")), "Are the lizards exposed to the sun or in the shade?", placement = "top"),
        soil,
        tipify(radioGroupButtons("loc", list(icon("map-pin"), "Location"), 
                           choices = c('<i class="fas fa-seedling"></i>' = "Ground", 
                                       '<i class="fas fa-tree"></i>' = "Tree"),
                           status = "primary", justified = TRUE),
                           # choicesOpt = list(icon = c("fa fa-seedling", "fa fa-tree")),
                           # options = list(style = "none")), 
               "Lizard on ground / on trees")
      )
    } else if(input$species == "Grasshopper") {
      list(
        h4(div(tags$img(src="Grasshopper_icon.png", height = 25), "Grasshopper")),
        numericInput("length", list(icon("ruler"), ("Length (mm)")), value = 50),
        tipify(sliderInput("abs_hopper", list(icon("tint"), "Absorptivity"), min = 0, max = 1, value = 0.6, step = 0.1), "Grasshopper absorptivity to solar radiation"),
        # selectInput("soil", list(icon("seedling"), "Soil surface"), 
        #             choices = c("Grass", "Dark soil", "Snow")),
                    # choicesOpt = list(icon = c("fa fa-pagelines", "fa fa-circle", "fa fa-snowflake")),
                    # options = list(style = "none")),
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
        h4(div(tags$img(src="Butterfly_icon.png", height = 20), "Butterfly")),
        numericInput("diam", list(icon("ruler-vertical"), "Thoracic diameter (mm)"), value = 3.6),
        numericInput("fur", list(icon("ruler-horizontal"), "Fur thickness (mm)"), value = 1.46),
        tipify(sliderInput("abs_butter", list(icon("tint"), "Absorptivity"), min = 0, max = 1, value = 0.6, step = 0.1), "Wing solar absorbtivity"),
        tipify(selectInput("shade", list(icon("umbrella-beach"), "Shade"), choices = c("Exposed", "Covered")), "Are the butterflies exposed to the sun or in the shade?", placement = "top"),
        soil,
        weather
      )
    } else if (input$species == "Snail") {
      list(
        h4(div(tags$img(src="Snail_icon.png", height = 25), "Snail")),
        weather,
        numericInput("length", list(icon("ruler"), "Length (mm)"), value = 12)
      )
    } else if (input$species == "Mussel") {
      list(
        h4(div(tags$img(src="Mussel_icon.png", height = 25), "Mussel")),
        numericInput("length", list(icon("ruler-horizontal"), "Length (cm)"), value = 10),
        numericInput("height", list(icon("ruler-vertical"), "Length (cm)"), value = 5),
        awesomeCheckbox("gape", "Are the mussels gaping?"),
        selectInput("group", "Group", choices = c("Aggregated", "Solitary")),
        weather
      )
    }
  })

  zenith <- reactive({
    validate(
      need(input$species %in% c("Lizard", "Grasshopper", "Butterfly", "Mussel"), "")
    )
    df <- rasterToPoints(r()) %>% as.data.frame() %>% dplyr::select("x", "y")
    
    df$zenith <- zenith_angle(doy = (which(input$monthAll %in% monthNames) - 1) * 30 + 15, lat = df$y, lon = df$x, hour = hour())
    df_raster <- df
    coordinates(df_raster) <- ~ x + y
    gridded(df_raster) <- TRUE
    crs(df_raster) <- "+proj=longlat +ellps=WGS84 +no_defs"
    zenith <- raster(df_raster)
    
    # if (input$year != 2020) {
    #   validate(
    #     need(airTemp(), "")
    #   )
    #   zenith <- resample(zenith, airTemp())
    # }
    zenith
  })
  
  albedo <- reactive({
    validate(
      # need(input$species %in% c("Grasshopper", "Lizard", "Butterfly"), ""),
      need(input$soil, "")
    )
    if(input$soil == "Grass") {
      albedo = 0.25 
    } else if (input$soil == "Dark soil") {
      albedo = 0.1
    } else {
      albedo = 0.75
    }
  })
  
  airTemp <- reactive({
    if (input$options == "gridMET") {
      if (input$year == 2020) {
        airTemp <- diurnal_temp_variation_sine(r()$tmax - 273.15, r()$tmin - 273.15, hour())
      } else {
        validate(need(input$scenario, ""))
        
        if (input$scenario == "Optimistic") {
          scn <- 26
        } else if (input$scenario == "Intermediate") {
          scn <- 60
        } else {
          scn <- 85
        }
        airTemp <- raster(paste0("year", input$year, "/rcp", scn, "/", input$monthAll, ".grd"))
      }
      
    } else if (input$options == "microclim") {
      # filename <- paste0("microclim/", input$monthAll, ".grd")
      # Ta <- brick(filename)
      # airTemp <- Ta[[hour()]]
      
      filename <- paste0("microclim_short/", input$monthAll, ".grd")
      airTemp <- raster(filename)
      
      if (input$year != 2020) {
        validate(
          need(input$scenario, "")
        )
        if (input$scenario == "Optimistic") {
          scn <- 26
        } else if (input$scenario == "Intermediate") {
          scn <- 60
        } else {
          scn <- 85
        }
        
        yearfile <- paste0("year", input$year, "dif/rcp", scn, "/", input$monthAll, ".grd")
        # dif <- resample(raster(yearfile), Ta)
        airTemp <- airTemp + raster(yearfile)
      }
    }
    airTemp
  })
  
  bodyTemp <- reactive({
    # if (input$year != 2020) {
    #   r <- resample(r, airTemp())
    #   elevData <- resample(elevData, airTemp())
    # }
    airTemp <- resample(airTemp(), elevData)
    
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
    
    if(input$species == "Lizard") {
      
      validate(
        need(input$soil, "")
      )
      
      sun <- ifelse(input$shade == "Exposed", TRUE, FALSE)
      
      surface <- ifelse(input$loc == "Ground", TRUE, FALSE)

      Tb <- Tb_lizard(T_a = airTemp,
                      T_g = airTemp + 5,
                      u = r()$wind, 
                      svl = input$svl, 
                      m = input$mass, 
                      psi = zenith(),
                      rho_S = albedo(), 
                      elev = elevData, 
                      doy = day_of_year(Sys.Date()), 
                      sun = sun, 
                      surface = surface,
                      alpha_S = 0.9,
                      alpha_L = 0.965,
                      F_d=0.5, 
                      F_r=0.5, 
                      F_a=0.5, 
                      F_g=0.5
                      )
      
    } else if(input$species == "Grasshopper") {
      
      Tb <- Tb_grasshopper(T_a = airTemp(), 
                           T_g = airTemp() + 5, 
                           u = r()$wind, 
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
                         u = r()$wind, 
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
                     WS = r()$wind,
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
                      u = r()$wind,
                      psi = zenith(),
                      evap = input$gape,
                      cl = CC,
                      group = group
                      )
    }
    
    Tb
  })

  output$mymap <- renderLeaflet({
    validate(
      need(bodyTemp(), "")
    )
    min <- minValue(bodyTemp())
    max <- maxValue(bodyTemp())
    int <- (max - min) / 5
    CTmax <- input$CTmax
    palette <- c('#DCECC4', '#4376c7', '#59711b', '#ffc324', '#ff7729')
    bins <- c()
    current = round(min)
    count = 0
    
    while(CTmax > current && max - 2 > current) {
      count = count + 1
      bins <- c(bins, current)
      current <- round(current + int)
    }
    
    if (input$red) {
      if(CTmax > max) {
        bins <- c(bins, round(max))
        pal_tb <- c(palette[0:count])
      } else {
        bins <- c(bins, round(max(min, CTmax)), round(max))
        pal_tb <- c(palette[0:count], "red")
      }
    } else {
      bins <- c(round(min), round(min + int), round(min + int * 2), round(min + int * 3), round(max - int), round(max))
      pal_tb <- palette
    }
    
    pal <- colorBin(palette = pal_tb, 
                    c(round(min), round(max)),
                    bins = bins,
                    na.color = "transparent"
    )
    
    validate(
      need(airTemp(), "")
    )
    minTa <- round(minValue(airTemp()))
    maxTa <- round(maxValue(airTemp()))
    intTa <- round((maxTa - minTa) / 5)
    pal_air <- colorBin(palette = palette,
                        c(minTa, maxTa),
                        bins = c(minTa, minTa + intTa, minTa + intTa * 2, minTa + intTa * 3, maxTa - intTa, maxTa),
                        na.color = "transparent"
    )
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addRasterImage(x = bodyTemp(), colors = pal, group = "Body temperatures", opacity = 0.6) %>%
      addRasterImage(x = airTemp(), colors = pal_air, group = "Air temperatures", opacity = 0.6) %>%
      setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
      addLayersControl(baseGroups = c("Body temperatures", "Air temperatures")) %>%
      addLegend(pal = pal,
                values = c(min, max),
                group = "Body legend",
                position = "bottomright",
                title = "Body temperatures (°C)")

  })
  
  observeEvent(input$mymap_groups, {
    
    min <- minValue(bodyTemp())
    max <- maxValue(bodyTemp())
    int <- (max - min) / 5
    CTmax <- input$CTmax
    palette <- c('#DCECC4', '#4376c7', '#59711b', '#ffc324', '#ff7729')
    bins <- c()
    current = round(min)
    count = 0
    
    while(CTmax > current && max - 2 > current) {
      count = count + 1
      bins <- c(bins, current)
      current <- round(current + int)
    }
    
    if (input$red) {
      if(CTmax > max) {
        bins <- c(bins, round(max))
        pal_tb <- c(palette[0:count])
      } else {
        bins <- c(bins, round(max(min, CTmax)), round(max))
        pal_tb <- c(palette[0:count], "red")
      }
    } else {
      bins <- c(round(min), round(min + int), round(min + int * 2), round(min + int * 3), round(max - int), round(max))
      pal_tb <- palette
    }
    
    pal <- colorBin(palette = pal_tb, 
                    c(round(min), round(max)),
                    bins = bins,
                    na.color = "transparent"
    )
    
    minTa <- round(minValue(airTemp()))
    maxTa <- round(maxValue(airTemp()))
    intTa <- round((maxTa - minTa) / 5)
    pal_air <- colorBin(palette = palette,
                        c(minTa, maxTa),
                        bins = c(minTa, minTa + intTa, minTa + intTa * 2, minTa + intTa * 3, maxTa - intTa, maxTa),
                        na.color = "transparent"
    )
    
    if(input$mymap_groups == "Body temperatures") {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = pal,
                  values = c(min, max),
                  group = "Body legend",
                  position = "bottomright",
                  title = "Body temperatures (°C)")
    } else {
      leafletProxy('mymap') %>% clearControls() %>%
        addLegend(pal = pal_air,
                  values = c(minTa, maxTa),
                  group = "Air legend",
                  position = "bottomright",
                  title = "Air temperatures (°C)")
    }
  })
  
  # observeEvent(input$year, {
  #   if (input$options != "gridMET" || input$year == 2020) {
  #     hide("offset")
  #   } else {
  #     show("offset")
  #   }
  # })
}


