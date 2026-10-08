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

Tests cover local GeoTIFF handling plus restriction import and
intersection behaviour (mixed geometry, holes, multipolygons, invalid or
empty files, label escaping, disjoint/contained/crossing/boundary-touch
cases, and wiring of the measure control and overlay toggle in both
editions).

## Distance measurement

The map has a ruler control (top-right). It reports metres with kilometres
alongside and stays separate from the survey area and mission calculations.
Use it for shore-to-area checks, start-point offsets, or any two points.
Finish or delete a measurement with the control itself; that never deletes
the survey area. The ruler works offline once the app has loaded. While
drawing the polygon, the draw tooltip also shows metric area. The installed
draw version has no per-edge length readout.

## Restriction zones (advisory import, no live feed)

This edition works offline and bundles no live restriction feed. Under
**No-fly / restriction zones**, import GeoJSON or KML polygons (up to
2000 features) from local files. They draw in a toggleable red overlay
that survives local-basemap load and removal. The panel warns when the
survey area intersects an imported boundary, including boundary contact,
after draws, edits, drags, imports, resizes, and rotations. Hiding the
overlay does not turn off warnings. Planning and export stay available.
States are distinct: no data loaded, no overlap (not permission to fly),
overlap warning, and check unavailable.

To check a plan in OK2Fly: use **Export AOI as KML**, open
[OK2Fly](https://ok2fly.com.au/), choose Load Geometry File, select the
KML, then run its flight check. OK2Fly Web geometry tools need a
subscription. Its airspace data is advisory and not CASR Part 175
approved; its terms prohibit scraping, reverse engineering, and
derivative reuse, so there is no documented live integration here.

Data-source findings, October 2026: OK2Fly publishes no restriction-layer
API or boundary download. CASA hosts no GIS restriction downloads and
points to verified
[drone safety apps](https://www.casa.gov.au/knowyourdrone/drone-safety-apps).
Airservices [Shape Files](https://data.airservicesaustralia.com/data-product/shape-files)
cost $5,950 AUD per year plus royalties and need a subscription, and
[Digital Facilities Maps](https://data.airservicesaustralia.com/data-product/digital-facilities-maps)
are limited to CASA RPAS Digital Platform participants on a trial basis.
The Department of Infrastructure
[Drone Rule Digitisation](https://www.drones.gov.au/policies-and-programs/initiatives/drone-rule-digitisation)
map covers parks and corrections (7,610 areas, 15 authorities, February
2024); its open `Drone_Rules_Australia_csv` endpoint is an attribute
table with no geometry. There is no single stable polygon download in
that set to bundle as authoritative no-fly zones. Import boundaries from
the Local Drone Rules map or the relevant council or parks open-data
portal, and confirm currency and permits before flying.
