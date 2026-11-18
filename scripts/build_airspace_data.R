# Builds the bundled advisory airspace layer used by airspace_check.R.
#
# Only zones relevant to CASA excluded-category / recreational standard
# operating conditions are kept:
#   * aerodrome no-fly zones built per CASA RPAS Platform Operating Rules
#     v4.0 Attachment C: 3 NM from the runway centreline plus approach and
#     departure paths (block at controlled aerodromes, advise elsewhere)
#   * helicopter landing sites, 0.75 NM (advise)
#   * restricted and prohibited areas whose lower limit is at or below
#     400 ft (no-fly unless authorised)
#   * DJI GEO restricted and authorization zones, where DJI firmware stops
#     the aircraft
#
# Data: openAIP (CC BY-NC-SA, https://www.openaip.net), OpenStreetMap runways
# (ODbL, https://www.openstreetmap.org/copyright), OurAirports runway ends
# (public domain) and DJI FlySafe GEO zones (https://fly-safe.dji.com).
#
# Usage (from the project root; the key is read from the environment and
# never written to disk):
#   OPENAIP_API_KEY=... Rscript scripts/build_airspace_data.R

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(sf)
})

api_key <- Sys.getenv("OPENAIP_API_KEY")
if (!nzchar(api_key)) stop("Set OPENAIP_API_KEY before running this script.")

`%||%` <- function(a, b) if (is.null(a)) b else a
out_dir <- file.path("data")
dir.create(out_dir, showWarnings = FALSE)
# Marine surveys: keep features offshore or within this distance of the coast.
coastal_inland_m <- 10000
# Towered aerodromes that openAIP models with no control zone or tower-hours
# zone of their own (checked against Airservices tower list).
controlled_override_icao <- c("YMEN")
max_relevant_lower_ft <- 400

fetch_openaip <- function(kind) {
  items <- list()
  page <- 1L
  repeat {
    # Query by country: Cloudflare rejects bbox queries with negative values.
    response <- GET(
      sprintf("https://api.core.openaip.net/api/%s", kind),
      query = list(country = "AU", limit = 1000, page = page),
      add_headers(`x-openaip-api-key` = api_key, Accept = "application/json"),
      timeout(120)
    )
    stop_for_status(response, paste("download openAIP", kind))
    body <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    items <- c(items, body$items)
    if (page >= body$totalPages) break
    page <- page + 1L
  }
  items
}

limit_feet <- function(limit) {
  value <- as.numeric(limit$value)
  switch(as.character(limit$unit), "0" = value / 0.3048, "1" = value, "6" = value * 100, NA_real_)
}
limit_reference <- function(limit) {
  switch(as.character(limit$referenceDatum), "0" = "GND", "1" = "MSL", "2" = "STD", "?")
}
limit_text <- function(limit) {
  ref <- limit_reference(limit)
  if (identical(ref, "GND") && as.numeric(limit$value) == 0) return("GND")
  unit <- switch(as.character(limit$unit), "0" = "m", "1" = "ft", "6" = "FL", "")
  if (identical(unit, "FL")) return(sprintf("FL%03d", as.integer(limit$value)))
  sprintf("%s %s %s", format(as.numeric(limit$value), big.mark = ","), unit, ref)
}
polygon_sfc <- function(geometry) {
  rings <- lapply(geometry$coordinates, function(ring) {
    do.call(rbind, lapply(ring, function(point) as.numeric(unlist(point))[1:2]))
  })
  st_make_valid(st_sfc(st_polygon(rings), crs = 4326))
}
local_crs <- function(lon, lat) {
  sprintf("+proj=aeqd +lat_0=%.8f +lon_0=%.8f +datum=WGS84 +units=m +no_defs", lat, lon)
}

message("Downloading openAIP airspaces and airports for Australia ...")
airspaces <- fetch_openaip("airspaces")
airports <- fetch_openaip("airports")
record_date <- function(items) max(vapply(items, function(x) substr(x$updatedAt, 1, 10), ""))
airspace_updated <- record_date(airspaces)
airports_updated <- record_date(airports)

# openAIP airspace types: 1 restricted, 3 prohibited, 4 control zone (CTR).
type_of <- vapply(airspaces, function(x) as.integer(x$type), 0L)

# --- Restricted and prohibited areas reachable at drone heights ---
special <- airspaces[type_of %in% c(1L, 3L)]
special <- Filter(function(x) {
  lower_ref <- limit_reference(x$lowerLimit)
  lower_ref %in% c("GND", "MSL") &&
    (identical(lower_ref, "GND") || limit_feet(x$lowerLimit) <= max_relevant_lower_ft)
}, special)
special_sf <- st_sf(
  category = vapply(special, function(x) if (x$type == 3) "prohibited" else "restricted", ""),
  name = vapply(special, function(x) x$name, ""),
  ident = "",
  lower_ft = vapply(special, function(x) {
    if (identical(limit_reference(x$lowerLimit), "GND")) 0 else round(limit_feet(x$lowerLimit))
  }, 0),
  lower_ref = vapply(special, function(x) limit_reference(x$lowerLimit), ""),
  limits = vapply(special, function(x) paste(limit_text(x$lowerLimit), "to", limit_text(x$upperLimit)), ""),
  geometry = do.call(c, lapply(special, function(x) polygon_sfc(x$geometry)))
)

# --- Controlled aerodromes: the main aerodrome inside each control zone ---
airport_type <- vapply(airports, function(x) as.integer(x$type), 0L)
airports <- airports[airport_type != 8L] # 8 = closed
airport_points <- st_sfc(lapply(airports, function(x) {
  st_point(as.numeric(unlist(x$geometry$coordinates))[1:2])
}), crs = 4326)
longest_runway_m <- vapply(airports, function(x) {
  lengths <- vapply(x$runways %||% list(), function(r) {
    len <- r$dimension$length
    if (is.null(len)) 0 else as.numeric(len$value) * if (identical(as.integer(len$unit), 1L)) 0.3048 else 1
  }, 0)
  if (length(lengths)) max(lengths) else 0
}, 0)

ctrs <- airspaces[type_of == 4L]
controlled_index <- integer()
for (ctr in ctrs) {
  inside <- which(lengths(st_within(airport_points, polygon_sfc(ctr$geometry))) > 0)
  if (!length(inside)) next
  controlled_index <- c(controlled_index, inside[which.max(longest_runway_m[inside])])
}
# Tower-hours zones name every part-time towered aerodrome, including those
# sharing a control zone (e.g. Richmond, Nowra, Pearce) and those without one.
zone_names <- vapply(airspaces[type_of == 6L], function(x) x$name, "")
tower_names <- zone_names[grepl(" TWR ", zone_names)]
tower_icao <- sub(".*\\((Y[A-Z]{3})\\).*", "\\1", tower_names[grepl("\\(Y[A-Z]{3}\\)", tower_names)])
airport_icao <- vapply(airports, function(x) x$icaoCode %||% "", "")
wanted_icao <- unique(c(tower_icao, controlled_override_icao))
missing_icao <- setdiff(wanted_icao, airport_icao)
if (length(missing_icao)) warning("Towered aerodromes not in openAIP airports: ", paste(missing_icao, collapse = ", "))
controlled_index <- unique(c(controlled_index, which(airport_icao %in% wanted_icao)))

# --- Runway centrelines from threshold points ---
# CASA RPAS Platform Operating Rules v4.0, Attachment C: zones derive from the
# runway centreline joining the thresholds. OpenStreetMap runway lines (ODbL)
# cover most aerodromes; OurAirports runway ends fill aerodromes OSM lacks.
message("Downloading OpenStreetMap runways and OurAirports runway ends ...")
overpass <- POST(
  "https://overpass-api.de/api/interpreter",
  body = list(data = '[out:json][timeout:240];way["aeroway"="runway"](-44.5,112,-9,160);out geom;'),
  encode = "form", user_agent("drone-survey-planner/1.0"), timeout(300)
)
stop_for_status(overpass, "download OpenStreetMap runways")
osm_ways <- fromJSON(content(overpass, "text", encoding = "UTF-8"), simplifyVector = FALSE)$elements
osm_lines <- lapply(osm_ways, function(way) {
  g <- way$geometry
  rbind(c(g[[1]]$lon, g[[1]]$lat), c(g[[length(g)]]$lon, g[[length(g)]]$lat))
})
runways <- read.csv("https://davidmegginson.github.io/ourairports-data/runways.csv",
                    stringsAsFactors = FALSE)
runways <- runways[runways$closed == 0 & stats::complete.cases(runways[, c(
  "le_longitude_deg", "le_latitude_deg", "he_longitude_deg", "he_latitude_deg")]), ]

# openAIP airport types 4 and 7 are military and civil heliports.
is_heliport <- vapply(airports, function(x) as.integer(x$type) %in% c(4L, 7L), TRUE)
fixed_wing <- which(!is_heliport)
assign_lines <- function(lines) {
  if (!length(lines)) return(integer())
  mids <- st_sfc(lapply(lines, function(m) st_point(colMeans(m))), crs = 4326)
  nearest <- st_nearest_feature(mids, airport_points[fixed_wing])
  near <- as.numeric(st_distance(mids, airport_points[fixed_wing][nearest], by_element = TRUE)) <= 3000
  ifelse(near, fixed_wing[nearest], NA_integer_)
}
osm_owner <- assign_lines(osm_lines)
runway_lines <- vector("list", length(airports))
for (k in which(!is.na(osm_owner))) {
  runway_lines[[osm_owner[k]]] <- c(runway_lines[[osm_owner[k]]], list(osm_lines[[k]]))
}
oa_lines <- lapply(seq_len(nrow(runways)), function(k) rbind(
  c(runways$le_longitude_deg[k], runways$le_latitude_deg[k]),
  c(runways$he_longitude_deg[k], runways$he_latitude_deg[k])))
oa_owner <- assign_lines(oa_lines)
for (i in unique(stats::na.omit(oa_owner))) {
  if (!length(runway_lines[[i]])) runway_lines[[i]] <- oa_lines[which(oa_owner == i)]
}

three_nm_m <- 5556
hls_m <- 1389
splay <- tan(15 * pi / 180)
ring <- function(m) st_polygon(list(rbind(m, m[1, ])))
trapezoid <- function(p, dir, n, d0, d1) {
  h0 <- 50 + d0 * splay
  h1 <- 50 + d1 * splay
  rbind(p + d0 * dir + h0 * n, p + d1 * dir + h1 * n, p + d1 * dir - h1 * n, p + d0 * dir - h0 * n)
}
aerodrome_zones <- function(i, controlled) {
  centre <- as.numeric(unlist(airports[[i]]$geometry$coordinates))[1:2]
  crs <- local_crs(centre[1], centre[2])
  back <- function(geoms) {
    if (!length(geoms)) return(NULL)
    shape <- st_union(st_sfc(geoms, crs = crs))
    st_transform(st_simplify(shape, dTolerance = 10), 4326)
  }
  zone <- list(); paths <- list(); upper <- list()
  for (m in runway_lines[[i]]) {
    xy <- st_coordinates(st_transform(st_sfc(st_multipoint(m), crs = 4326), crs))[, 1:2]
    a <- xy[1, ]; b <- xy[2, ]
    len <- sqrt(sum((b - a)^2))
    if (len < 50) next
    u <- (b - a) / len
    n <- c(-u[2], u[1])
    zone <- c(zone, list(st_buffer(st_linestring(rbind(a, b)), three_nm_m, nQuadSegs = 30)))
    paths <- c(paths, list(
      ring(rbind(a - 2000 * u + 500 * n, b + 2000 * u + 500 * n,
                 b + 2000 * u - 500 * n, a - 2000 * u - 500 * n)),
      ring(trapezoid(a, -u, n, 0, 7000)), ring(trapezoid(b, u, n, 0, 7000))))
    if (controlled) upper <- c(upper, list(
      ring(trapezoid(a, -u, n, 7000, 8500)), ring(trapezoid(b, u, n, 7000, 8500))))
  }
  if (!length(zone)) {
    # Method B: no thresholds, so buffer the aerodrome reference point.
    zone <- list(st_buffer(st_point(c(0, 0)), three_nm_m, nQuadSegs = 30))
  }
  list(zone = back(zone), paths = back(paths), upper = back(upper))
}

zone_rows <- list()
add_zone <- function(geometry, category, i, lower_ft, limits) {
  if (is.null(geometry)) return(invisible())
  zone_rows[[length(zone_rows) + 1L]] <<- st_sf(
    category = category, name = airports[[i]]$name, ident = airports[[i]]$icaoCode %||% "",
    lower_ft = lower_ft, lower_ref = "GND", limits = limits, geometry = geometry)
}
for (i in seq_along(airports)) {
  if (is_heliport[i]) {
    centre <- as.numeric(unlist(airports[[i]]$geometry$coordinates))[1:2]
    crs <- local_crs(centre[1], centre[2])
    add_zone(st_transform(st_buffer(st_sfc(st_point(c(0, 0)), crs = crs), hls_m, nQuadSegs = 20), 4326),
             "heliport", i, 0, "0.75 NM of a helicopter landing site")
    next
  }
  controlled <- i %in% controlled_index
  zones <- aerodrome_zones(i, controlled)
  add_zone(zones$zone, if (controlled) "controlled_aerodrome" else "aerodrome", i, 0,
           "3 NM of the runway")
  add_zone(zones$paths, if (controlled) "controlled_approach" else "aerodrome_approach", i, 0,
           "Approach and departure paths (to 7 km)")
  add_zone(zones$upper, "controlled_approach_upper", i, 300,
           "Outer approach and departure paths (7-8.5 km), 300-400 ft AGL")
}
aerodrome_sf <- do.call(rbind, zone_rows)
controlled_sf <- aerodrome_sf[aerodrome_sf$category == "controlled_aerodrome", ]

# --- DJI GEO zones: where DJI firmware stops the aircraft ---
# Same (undocumented) service as DJI's FlySafe map. Levels 1 (authorization,
# self-unlock) and 2 (restricted, custom unlock) stop the aircraft; warning
# levels (0, 3) only prompt and are not kept.
message("Downloading DJI GEO zones ...")
dji_areas <- list()
for (top in seq(-9, -42, by = -3)) for (left in seq(112, 157, by = 3)) {
  response <- tryCatch(GET(
    "https://flysafe-api.dji.com/api/qep/geo/feedback/areas/in_rectangle",
    query = list(zones_mode = "flysafe_website", drone = "dji-mavic-3-enterprise",
                 level = "0,1,2,3,4,5,6,7,8,9,10", ltlat = top, ltlng = left,
                 rblat = top - 3, rblng = left + 3),
    user_agent("Mozilla/5.0"), timeout(60)), error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200) {
    stop("DJI GEO zone download failed at ", top, ", ", left)
  }
  body <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  for (area in body$data$areas) dji_areas[[as.character(area$area_id)]] <- area
  Sys.sleep(0.2)
}
dji_rows <- list()
for (area in dji_areas) {
  if (!identical(area$country, "AU")) next
  subs <- area$sub_areas
  if (!length(subs)) subs <- list(area)
  for (sub in subs) {
    level <- as.integer(sub$level)
    if (!level %in% c(1L, 2L)) next
    geometry <- if (length(sub$polygon_points)) {
      polygon_sfc(list(coordinates = sub$polygon_points))
    } else {
      crs <- local_crs(sub$lng, sub$lat)
      st_transform(st_buffer(st_sfc(st_point(c(0, 0)), crs = crs), sub$radius, nQuadSegs = 20), 4326)
    }
    dji_rows[[length(dji_rows) + 1L]] <- st_sf(
      category = if (level == 2L) "dji_restricted" else "dji_authorization",
      name = area$name, ident = paste0("DJI ", area$area_id),
      lower_ft = round(as.numeric(sub$height %||% 0) / 0.3048), lower_ref = "GND",
      limits = if (level == 2L) "DJI restricted zone" else "DJI authorization zone",
      geometry = geometry)
  }
}
dji_sf <- do.call(rbind, dji_rows)

layer <- rbind(aerodrome_sf, special_sf, dji_sf)
st_geometry(layer) <- st_cast(st_geometry(layer), "GEOMETRY")

# --- Keep only the coastal band used for marine surveys ---
# Drop features lying wholly more than coastal_inland_m inside the mainland or
# a large island; everything offshore or near the coast is kept. Islands
# missing from the land data count as sea, so their features are kept.
message("Downloading Natural Earth 10 m land and clipping to the coastal band ...")
land <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical",
                                   returnclass = "sf")
sf_use_s2(FALSE)
land <- suppressWarnings(st_crop(st_make_valid(land), c(xmin = 110, ymin = -46, xmax = 160, ymax = -8)))
sf_use_s2(TRUE)
albers <- 3577 # GDA94 / Australian Albers (metres)
inland_core <- st_union(st_buffer(st_transform(land, albers), -coastal_inland_m))
deep_inland <- lengths(st_within(st_transform(layer, albers), inland_core)) > 0
message(sprintf("Coastal filter dropped %d of %d features.", sum(deep_inland), nrow(layer)))
layer <- layer[!deep_inland, ]
geojson_path <- file.path(out_dir, "airspace_au.geojson")
unlink(geojson_path)
st_write(layer, geojson_path, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=6", "RFC7946=YES"))

meta <- list(
  source = "openAIP, OpenStreetMap runways, OurAirports runway ends, DJI FlySafe GEO zones",
  dji_downloaded = format(Sys.Date()),
  licence = "openAIP data CC BY-NC-SA (Data used comes from openAIP); runways (c) OpenStreetMap contributors, ODbL; DJI zones (c) DJI.",
  airspace_updated = airspace_updated,
  airports_updated = airports_updated,
  downloaded = format(Sys.Date()),
  coastal_inland_m = coastal_inland_m,
  rules = "CASA excluded-category / recreational standard operating conditions",
  counts = as.list(table(layer$category))
)
write_json(meta, file.path(out_dir, "airspace_au_meta.json"), auto_unbox = TRUE, pretty = TRUE)
message(sprintf("Wrote %d features (%s); airspace records updated %s; controlled: %s.",
                nrow(layer), paste(names(meta$counts), unlist(meta$counts), sep = "=", collapse = ", "),
                airspace_updated, paste(sort(controlled_sf$ident), collapse = " ")))
