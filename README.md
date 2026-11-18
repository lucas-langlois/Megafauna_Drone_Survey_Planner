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
may be reduced, and image footprints can extend beyond the boundary. While
placing, the map shows a crosshair; **Cancel** stops placement without moving
the pilot. **Remove pilot** clears the marker so you can place it again. Untick
the option to restore the route without the pilot-radius limit. The existing 1.8 km trimming
option can be used alongside the radius limit.

The radius is a planning aid; confirm actual visual line of sight and visibility
of the complete route. It does not set the aircraft's home or takeoff point.

## Airspace check (advisory)

The map has three overlays, each switched on or off in the layer control:

- **DJI GEO zones (drone will stop)**, in purple: DJI FlySafe restricted and
  authorization zones. In Australia these are mostly circles around airports,
  such as 670 m at Bowen and 1.38 km at Townsville. DJI firmware stops the
  aircraft at the boundary, and the mission aborts unless the zone is
  unlocked.
- **CASA no-fly zones (advisory)**, in red, covering:
  - the 3 NM (5.56 km) zone around each controlled (towered) aerodrome
  - its runway approach and departure paths
  - restricted areas that start at ground level
- **CASA caution zones (advisory)**, in dashed orange, covering:
  - the 3 NM zone around each non-controlled aerodrome, such as Bowen
  - its runway approach and departure paths
  - 0.75 NM (1.39 km) around helicopter landing sites

The aerodrome zones are built to the method that CASA-verified apps such as
OK2Fly must use. That method is set out in Attachment C of CASA's
[RPAS Platform Operating Rules v4.0](https://www.casa.gov.au/sites/default/files/2025-02/rpas-platform-operating-rules-commencing-october-2025.pdf):

- **3 NM zone:** measured from the runway centreline between the thresholds.
- **Approach and departure paths:** a rectangle 500 m either side of the
  centreline, running 2 km past each threshold, joined to trapezoids. Each
  trapezoid starts 100 m wide at the threshold and splays out at 15° to 7 km.
- **Controlled aerodromes only:** the trapezoids continue to 8.5 km. That
  outer part only applies at or above 300 ft.

The check uses the rules for the excluded category, which are the same as the
recreational rules: block at controlled aerodromes, advise at non-controlled
aerodromes and helicopter landing sites.

The survey area and the full route, including the connectors between
transects, are checked at the current flight height. Every zone the plan
touches is listed under the mission metrics, in the HTML plan summary and in
the Litchi bundle's text summary, most restrictive first:

- **DJI will stop:** the plan enters a DJI GEO zone. Unlock it through DJI
  before flying, or the mission aborts.
- **CASA no-fly:** the plan enters a controlled aerodrome's zone or paths, or
  a restricted area. A restricted area marked `[NOTAM]` is only active when a
  NOTAM activates it; check NOTAMs for the flight date.
- **Caution:** the plan enters a non-controlled aerodrome's zone or paths, or
  a helicopter landing site's zone. Flying is allowed only while no crewed
  aircraft are operating there.

### GBRMPA drone exclusion areas

A fifth overlay, **GBRMPA drone exclusion areas (permit needed)**, shown in
green, maps Table 6 ("Aircraft and drone exclusion areas") of the Reef
Authority's *Location-specific assessment* information sheet (3 Dec 2024).
Drones must not be flown there without specific Reef Authority permission.
Each listed island, cay, rock or reef is buffered by 1 km, the lateral
distance the sheet sets for aircraft. These areas are treated as no-fly: the
check reports them, and clipping removes them from the route. Seasonal
entries (1 Sept or 1 Oct to 31 Mar) are applied all year, because the
planner does not know the flight date. The period is shown in each finding.

The table is in `data/gbrmpa_drone_exclusions.csv`. Its features come from
GBRMPA's GBR Features layer, served by Geoscience Australia; no key is
needed. To rebuild, run `Rscript scripts/build_gbrmpa_exclusions.R`.

The Hinchinbrook Island Dugong Protection Area is mapped as the area itself,
with no buffer. Its outline combines GBRMPA's Special Management Areas layer,
which covers Marine Park waters only, with Queensland's Dugong Protection
Area A, which adds Queensland waters such as Hinchinbrook Channel. The
Queensland outline comes from a public copy published by the Port of
Townsville. Other table entries without a Reef ID cover whole areas and are not
mapped yet. These include all intertidal areas in the Far Northern
Management Area, the Whitsunday setting 5 areas, the Palm Island zones, the
coast between the Daintree and Bloomfield rivers, and rivers and inlets
between Adelaide Point and Midge Point. Check the sheet for these.

### Excluding land

Land is always excluded (Route > Route limits > "Always applied").
It removes the parts of survey lines over land, including islands, using
`data/land_mask/australia_land_mask_from_dea.shp`: an Australian land mask
derived from Digital Earth Australia, in GDA94 / Australian Albers. Transit
legs between survey blocks may still cross land, because flying over land is
not restricted. The mission metrics and summaries report how many lines were
cut and how much survey line over land was removed.

### Keeping the route out of no-fly zones

No-fly zones are always avoided (Route > Route limits > "Always applied"). It removes the parts
of transects that lie inside DJI GEO zones and CASA no-fly zones, at the
current flight height, with a 30 m margin. Caution zones and marine parks are
not clipped.

When a zone splits the survey area into separate blocks, every remaining
piece is still flown. The route flies each block back and forth in turn. When
a straight leg between blocks would cross a zone, the route goes around it,
using waypoints just outside the zone. The mission metrics, plan summary and
Litchi text summary report how much survey line was cut, how many blocks the
route flies, and the extra distance of any detours.

The outer approach paths of controlled aerodromes, 7 to 8.5 km out, are
no-fly only at 300 ft and above. They are drawn dotted light blue. As in OK2Fly
("MAX 91M"), they are clipped only when the flight height is above 91 m, so
fly at 91 m or lower to survey under them.

The layer covers the **coastal band only**, because the planner is for
marine surveys. It keeps everything offshore, on islands, or within 10 km
inland of the coast. Features further inland, such as Canberra or Alice
Springs, are dropped. Some estuary-side airports are dropped too, such as
Perth and Melbourne (Tullamarine): the coastline data treats wide rivers and
bays as land, so these count as more than 10 km inland.

This check is **not** CASA-verified. Its data comes from:

- openAIP (airspace and aerodromes)
- OpenStreetMap (runway ends), with OurAirports filling gaps
- DJI's FlySafe service, which is undocumented and may change

It is a static snapshot, and its dates are shown with every result. openAIP
lists few helicopter landing sites, so hospital and private helipads may be
missing. The check also leaves out marine parks; CASA's rules advise against
flying below 500 ft over a marine park unless you have approval. The app
never reports a plan as clear. Before every flight, confirm the area in a
CASA-verified drone safety app and in DJI FlySafe.

### Refreshing the data

The layer is bundled as `data/airspace_au.geojson`, together with
`data/airspace_au_meta.json`, so the app needs no API key. To rebuild it from
the latest openAIP data, get a free openAIP API key, then run this from the
project root:

```sh
OPENAIP_API_KEY=your-key Rscript scripts/build_airspace_data.R
```

The rebuild also downloads OpenStreetMap runways, OurAirports runway ends
and DJI GEO zones, and takes a few minutes. Never commit the key.

Licences: openAIP data is CC BY-NC-SA ("Data used comes from openAIP"), so
non-commercial use only, with attribution. Runway data is (c) OpenStreetMap
contributors, ODbL. GEO zones come from DJI FlySafe.

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
