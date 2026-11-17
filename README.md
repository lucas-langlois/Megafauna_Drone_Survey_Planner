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

## Distance measurement

The map has a ruler control (top-right). It reports metres with kilometres
alongside and stays separate from the survey area and mission calculations.
Use it for shore-to-area checks, start-point offsets, or any two points.
Finish or delete a measurement with the control itself; that never deletes
the survey area. While drawing the polygon, the draw tooltip also shows
metric area. The installed draw version has no per-edge length readout.

## Stationary pilot radius

Enable **Stationary pilot: limit route to pilot radius**, enter the maximum
distance in metres (900 m by default), then select **Place / move pilot on map**
and click the pilot's location. A purple boundary shows the selected radius.
Repeat placement to move the pilot, or cancel placement to keep the current point.

Survey lines are clipped to this boundary, including the extra coverage beyond
the survey polygon. Lines outside it are removed. The map, photo centres,
mission metrics, DJI and Litchi route exports, and HTML summary use the clipped
route. The original survey polygon and its KML export remain unchanged. Coverage
may be reduced, and image footprints can extend beyond the boundary. Click
**Remove pilot trim** or disable the option to restore the route without the
pilot-radius limit. The existing 1.8 km trimming
option can be used alongside the radius limit.

The radius is a planning aid; confirm actual visual line of sight and visibility
of the complete route. It does not set the aircraft's home or takeoff point.

## Overlap in the plan summary

The exported HTML plan summary records forward overlap and sidelap as
percentages. Forward overlap is the selected target for consecutive images along
a transect.
Sidelap describes adjacent image swaths: 0% means edge-to-edge footprints;
negative values indicate a gap between swaths. These values use the selected
camera geometry and planned flight height.

## Export names and folders

**Download complete plan** saves one ZIP containing the DJI waypoint KMZ,
Litchi bundle (CSV, native .lchz plan and import notes), HTML plan summary,
original survey-area KML, and a README explaining the files. Extract the ZIP
before using its contents. The included instructions specify transferring the
DJI and extracted Litchi plans to the drone RC via SD card.

Enter an optional **Export name** to name all exports, including files inside
the Litchi bundle. The app adds the drone model, file type and timestamp.
Leave the name empty to use the default filenames.

Where the browser supports folder access (such as desktop Chrome and Edge),
**Choose export folder** lets you grant access to a local folder for this tab's
session. Exports then save there directly. Existing files receive a numbered
suffix rather than being overwritten. **Use browser downloads** restores normal
downloads. Starting fresh also clears the folder selection.

Safari, Firefox and other browsers without folder access use standard downloads
with the same custom names. Enable the browser's **Ask where to save** download
setting to choose the destination for each export. The app cannot force that
setting or access a local folder without browser permission.

Deployment must include `app.R`, `pilot_radius.R`, and `www/export-downloads.js`.
