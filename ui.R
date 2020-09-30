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
library("RCurl")
library("stringr")
library("sf")
library("shinysky")

monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


shinyUI <-
  fluidPage(
    theme = shinytheme("united"),
    setBackgroundColor(color = "#F5F5F5"), 
    useShinyjs(),
    title = "Biophysical model map",
    titlePanel("Biophysical model map"),
    hr(),
    p("This map displays the operative temperature of lizards, grasshoppers, salamanders, butterflies, snails, and mussels across the United States for 2020, 2050, 2070, and 2090 using a future temperature projection model."),
    includeHTML("intro.html"),

    br(),
    fluidRow(
      column(6, radioGroupButtons("year", "Year", choices = c("Recent", "Near-term forecast", 2050, 2070, 2090), selected = NA, status = "success", size = "sm", justified = TRUE))
    ),
    
    uiOutput(outputId = "futureUI"),
    uiOutput(outputId = "manualUI"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("species", list(icon("paw"), "Species"), choices = c("Lizard", "Grasshopper", "Salamander", "Butterfly", "Snail", "Mussel")),
        #selectInput("monthAll", list(icon("calendar-alt"), "Month"), choices = monthNames),
        uiOutput(outputId = "hourUI"),
        numericInput("CTmax", list(icon("thermometer-half"), "Critical thermal maximum (°C)"), value = 40),
        materialSwitch("red", status = "danger", label = "Show areas above CTmax in red"),
        hr(),
        uiOutput(outputId = "dynamicUI")
      ),
      
      mainPanel(
        #sliderInput("offset", "Warming offset (°C)", min = -5, max = 5, value = 0, step = 0.5),
        htmlOutput("title"),
        leafletOutput("mymap") %>% withSpinner(type = 7),
      )
    ),
    
    bsTooltip("CTmax", "Oragnisms are in danger above this temperature"),
    bsTooltip("offset", "Use this to impose climate warming/cooling")
  )

