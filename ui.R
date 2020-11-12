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
# library("shinysky")
library("miniUI")

monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


shinyUI <- fluidPage(
  theme = shinytheme("united"),
  setBackgroundColor(color = "#F5F5F5"), 
  useShinyjs(),
  title = "Biophysical model map",
  titlePanel("Ectotherm body temperatures and thermal stress nowcasts and forecasts"),
  hr(),
  
  tabsetPanel(
    tabPanel("App description",
      includeHTML("intro.html"),
             ),
    tabPanel("Map",
      includeHTML("instruction.html"),
      br(),
      hr(),
      fluidRow(
        column(6, radioGroupButtons("year", "See temperature for...", choices = c("Recent", "This week", 2050, 2070, 2090), selected = NA, status = "success", size = "sm", justified = TRUE))
      ),
      
      uiOutput(outputId = "futureUI"),
    
      sidebarLayout(
        sidebarPanel(
          selectInput("species", list(icon("paw"), "Species"), choices = c("Lizard", "Grasshopper", "Salamander", "Butterfly", "Snail", "Mussel")),
          #selectInput("monthAll", list(icon("calendar-alt"), "Month"), choices = monthNames),
          uiOutput(outputId = "hourUI"),
          uiOutput(outputId = "tmaxUI"),
          hr(),
          uiOutput(outputId = "dynamicUI")
        ),
        
        mainPanel(
          switchInput(inputId = "scale", label = "Scale", onLabel = "Discrete", offLabel = "Continuous", inline = TRUE, value = TRUE, size = "small"),
          #sliderInput("offset", "Warming offset (°C)", min = -5, max = 5, value = 0, step = 0.5),
          htmlOutput("title"),
          leafletOutput("mymap") %>% withSpinner(type = 7),
        )
      )
    )
  ),
  
  bsTooltip("CTmax", "Oragnisms are in danger above this temperature"),
  bsTooltip("offset", "Use this to impose climate warming/cooling")
)

