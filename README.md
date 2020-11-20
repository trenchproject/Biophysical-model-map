# RShiny_BiophysicalModelMap

BiophysicalModelMap is an interactive shiny app that allows users to visualize the current and future operative temperatures of organisms. The app covers lizards, grasshoppers, butterflies, snails, mussels, and salamandars using the biophysical model functions from [TrenchR](https://github.com/trenchproject/TrenchR). Future temperatures are obtained from [CMIP5 multimodel ensemble](https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Welcome) with RCP2.6, RCP6.0 and RCP8.5 projections, which are considered as optimistic, intermediate and pessimistic scenarios, respectively.
We use ```climateR::getGridMET``` to obtain recent weather data and [Climate Forecast System](https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/climate-forecast-system-version2-cfsv2) for near-term forecast data.


## Prerequisites for opening in Rstudio
Git and Rstudio ([Instructions](https://resources.github.com/whitepapers/github-and-rstudio/))  
Installation of the following R packages: shiny, raster, leaflet, shinyWidgets, shinythemes, shinycssloaders, magrittr, shinyBS, shinyjs, rgdal, climateR, AOI, RCurl, stringr, sf

```
pkgs <- c("shiny", "raster", "leaflet", "shinyWidgets", "shinythemes", "shinycssloaders", "magrittr", "shinyBS", "shinyjs", "rgdal", "RCurl", "stringr", "sf")
lapply(pkgs, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

devtools::install_github(c("mikejohnson51/AOI", "mikejohnson51/climateR"))
```

## Using BiophysicalModelMap
* Opening in Rstudio:  
Click on "Code" on the top right to copy the link to this repository.  
Click ```File```, ```New Project```, ```Version Control```, ```Git```  
Paste the repository URL and click ```Create Project```.

* Alternatively, go to [this link](https://map.trenchproject.com/RShiny_BiophysicalModelMap/).

## Contributing to BiophysicalModelMap
<!--- If your README is long or you have some specific process or steps you want contributors to follow, consider creating a separate CONTRIBUTING.md file--->
To contribute to BiophysicalModelMap, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin <project_name>/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).

