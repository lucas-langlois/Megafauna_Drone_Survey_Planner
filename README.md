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

## Restriction zones (advisory import, no live feed)

The planner does not bundle a live nationwide no-fly feed. Under
**No-fly / restriction zones**, import your own GeoJSON or KML polygons
(up to 2000 features). They draw in a toggleable red overlay. The panel
warns when the survey area intersects an imported boundary, including
boundary contact, after draws, edits, drags, imports, resizes, and
rotations. Hiding the overlay does not turn off warnings. Planning and
export stay available. Three states are distinct: no data loaded, no
overlap (not permission to fly), and overlap warning. A failed check
shows as unavailable, not as zero overlap.

To check a plan in OK2Fly: use **Export AOI as KML**, open
[OK2Fly](https://ok2fly.com.au/), choose Load Geometry File, select the
KML, then run its flight check. OK2Fly Web geometry tools need a
subscription. Its airspace data is advisory and not CASR Part 175
approved; its terms prohibit scraping, reverse engineering, and
derivative reuse, so there is no documented live integration here.

Data-source findings, October 2026:

- OK2Fly publishes user-track/polygon GeoJSON/KML import and export in
  its docs. It does not publish a restriction-layer API or boundary
  download.
- CASA does not host GIS restriction downloads. It points to verified
  [drone safety apps](https://www.casa.gov.au/knowyourdrone/drone-safety-apps).
- Airservices [Shape Files](https://data.airservicesaustralia.com/data-product/shape-files)
  cost $5,950 AUD per year plus royalties and need a subscription.
  [Digital Facilities Maps](https://data.airservicesaustralia.com/data-product/digital-facilities-maps)
  are GeoJSON but limited to CASA RPAS Digital Platform participants on
  a trial basis.
- The Department of Infrastructure
  [Drone Rule Digitisation](https://www.drones.gov.au/policies-and-programs/initiatives/drone-rule-digitisation)
  map covers parks and correctional facilities (7,610 areas, 15
  authorities, first release February 2024). Its open
  `Drone_Rules_Australia_csv` endpoint is an attribute table with no
  geometry, correctional points date to a 2012 digitisation, and the
  department notes the API is still changing. There is no single stable
  polygon download in that set that this planner can bundle as
  authoritative no-fly zones.

Get importable boundaries from the Local Drone Rules map, the relevant
council or parks open-data portal, or your own advisory buffers, and
confirm currency and permit conditions before flying.
