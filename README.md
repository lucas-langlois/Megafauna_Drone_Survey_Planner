# Megafauna Drone Survey Planner

An R Shiny app for planning megafauna drone surveys. Define a survey area on
a map, calculate transects and flight metrics, and export mission files.

## Run locally

Install R and the required packages:

```r
install.packages(c(
  "shiny", "leaflet", "geosphere", "dplyr", "sf", "leaflet.extras",
  "shinyjs", "zip"
))
```

Open `Megafauna_Drone_Survey_Planner.Rproj` in RStudio, or set this directory
as your working directory, then run:

```r
shiny::runApp()
```

## Offline basemap edition

`offline_basemap_version/` contains an edition that supports local GeoTIFF
basemaps. See its [README](offline_basemap_version/README.md) for dependencies,
usage, and tests. Local imagery and survey data are excluded from this repository.

## Hosted app

The project's recorded shinyapps.io deployment is available at
[Megafauna Drone Survey Planner](https://lucas-langlois-jcu.shinyapps.io/Megafauna_Drone_Survey_Planner/).
Pushing code to GitHub does not update that deployment.
