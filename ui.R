# packages <- c("shiny", "raster", "leaflet", "shinyWidgets", "shinythemes", "shinycssloaders", "magrittr", "shinyBS", "shinyjs")
# 
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

library("shiny")
library("raster")
library("leaflet")
library("shinyWidgets")
library("shinythemes")
library("shinycssloaders")
library("magrittr")
library("shinyBS")
library("shinyjs")
library("rgdal")
#devtools::install_github(c("mikejohnson51/AOI", "mikejohnson51/climateR"))
library("climateR")
library("AOI")


hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


shinyUI <-
  fluidPage(
    theme = shinytheme("united"),
    setBackgroundColor(color = "#F5F5F5"), 
    useShinyjs(),
    titlePanel("Biophysical model map"),
    hr(),
    p("This map shows the operative tempearture of lizards, grasshoppers, salamanders, butterflies, snails and mussels across the United States based on the weather data from 2020 (two days ago), 2050, 2070 or in 2090."),
    includeHTML("intro.html"),
    br(), 
    hr(),
    radioGroupButtons("options", "gridMET or microclim", choices = c("gridMET", "microclim"), justified = TRUE),
    p("gridMET uses gridMET to get climate data. Daily maximum and minimum temperatures of the 15th of the 
      selected month from the past 12 months are used as the 2020 air temperature after converted to daily temperatures.
      When future years are selected, it uses data obtained from CMIP5 model of that year, and directly uses them as air temperatures."),
    p("microclim uses the microclim data, which are average monthly temperatures between 1961-1990. When future years
      are selected, it adds anomaly to those average temperatures, which are the difference between the temperature from
      the year selected and that from 2020. For the current version, it only uses temperatures at 1pm due to the
      data storage limit."),
    br(),
    fluidRow(
      column(6, radioGroupButtons("year", "Year", choices = c(2020, 2050, 2070, 2090), status = "success", size = "sm", justified = TRUE))
    ),
    uiOutput(outputId = "future"),
    # fluidRow(
    #   column(6, radioGroupButtons("scenario", "Scenarios", c("Optimistic", "Intermediate", "Pessimistic"), status = "danger", size = "sm", justified = TRUE)),
    #   column(6, selectInput("month", "Month", choices = monthNames))
    # ),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("species", list(icon("paw"), "Species"), choices = c("Lizard", "Grasshopper", "Salamander", "Butterfly", "Snail", "Mussel")),
        selectInput("monthAll", list(icon("calendar-alt"), "Month"), choices = monthNames),
        selectInput("hour", list(icon("glyphicon glyphicon-time", lib = "glyphicon"), "Hour"), choices = hours, selected = "01 PM"),
        numericInput("CTmax", list(icon("thermometer-half"), "Critical thermal maximum (°C)"), value = 40),
        materialSwitch("red", status = "danger", label = "Show areas above CTmax in red"),
        hr(),
        uiOutput(outputId = "dynamicUI")
      ),
      
      mainPanel(
        #sliderInput("offset", "Warming offset (°C)", min = -5, max = 5, value = 0, step = 0.5),
        leafletOutput("mymap") %>% withSpinner(type = 7),
      )
    ),
    
    bsTooltip("CTmax", "Oragnisms are in danger above this temperature"),
    bsTooltip("offset", "Use this to impose climate warming/cooling")
  )

