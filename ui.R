packages <- c("shiny", "raster", "leaflet", "shinyWidgets", "shinythemes", "shinycssloaders", "magrittr", "shinyBS")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#devtools::install_github(c("mikejohnson51/AOI", "mikejohnson51/climateR"))
library("climateR")



hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

shinyUI <-
  fluidPage(
    theme = shinytheme("united"),
    setBackgroundColor(color = "#F5F5F5"), 
    
    titlePanel("Biophysical model map"),
    hr(),
    p("This map shows the operative tempearture of lizards, grasshoppers, salamanders, butterflies, snails and mussels across the United States based on the weather data from two days ago."),
    p("The offset value can be used to simulated climate change."),
    sidebarLayout(
      sidebarPanel(
        selectInput("species", list(icon("paw"), "Species"), choices = c("Lizard", "Grasshopper", "Salamander", "Butterfly", "Snail", "Mussel")),
        selectInput("hour", list(icon("glyphicon glyphicon-time", lib = "glyphicon"), "Hour"), choices = hours, selected = "01 PM"),
        numericInput("CTmax", list(icon("thermometer-half"), "Critical thermal maximum (°C)"), value = 40),
        materialSwitch("red", status = "danger", label = "Show areas above CTmax in red"),
        hr(),
        uiOutput(outputId = "dynamicUI")
      ),
      
      mainPanel(
        sliderInput("offset", "Warming offset (°C)", min = -5, max = 5, value = 0, step = 0.5),
        leafletOutput("mymap") %>% withSpinner(type = 7),
      )
    ),
    
    bsTooltip("CTmax", "Oragnisms are in danger above this temperature"),
    bsTooltip("offset", "Use this to impose climate warming/cooling")
  )
