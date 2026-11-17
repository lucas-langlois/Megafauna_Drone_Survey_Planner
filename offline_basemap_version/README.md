# Megafauna Drone Survey Planner — offline basemap edition

This folder is the packaged local edition of the Shiny survey planner. It can
load one local `.tif`/`.tiff`, or multiple adjacent GeoTIFF tiles, as a temporary
Leaflet basemap without relying on internet tile services.

The online-basemap edition retained for shinyapps.io deployment remains one
directory above this folder.

## Install dependencies

```r
install.packages(c(
  "shiny", "leaflet", "geosphere", "dplyr", "sf", "leaflet.extras",
  "leafem", "shinyjs", "zip", "terra", "jsonlite", "later",
  "htmlwidgets", "testthat"
))
```

`terra` 1.6-3 or newer is required.

## Run

Open `Megafauna_Drone_Survey_Planner_Offline.Rproj`, or set this folder as the
working directory, then run:

```r
shiny::runApp()
```

Under **Offline Basemap**, choose **Load Local GeoTIFF Tile(s)** and select one
or more files. Select **Local GeoTIFF** in the map layer control if needed. Use
**Remove Local Basemap** to return to the online Satellite layer.

Files remain in Shiny's session upload directory. Adjacent tiles must have
matching CRS, pixel resolution, alignment, and band count. The browser requests
only the TIFF blocks required for the visible area; source files are not
modified or reduced in resolution.

The local workspace includes a validated 16-tile fixture set in
`global_quarterly_2026q2_mosaic/`. These imagery files are excluded from Git.
When cloning the repository, supply your own GeoTIFF files for local use.

## Test

From this folder, run:

```powershell
Rscript tests/testthat.R
```

Tests cover local GeoTIFF handling and the metric ruler control in both editions.

## Distance measurement

The map has a ruler control (top-right). It reports metres with kilometres
alongside and stays separate from the survey area and mission calculations.
Use it for shore-to-area checks, start-point offsets, or any two points.
Finish or delete a measurement with the control itself; that never deletes
the survey area. The ruler works offline once the app has loaded. While
drawing the polygon, the draw tooltip also shows metric area. The installed
draw version has no per-edge length readout.
