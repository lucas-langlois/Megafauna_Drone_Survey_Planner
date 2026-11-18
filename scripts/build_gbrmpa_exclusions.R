# Builds data/gbrmpa_exclusions.geojson: GBRMPA aircraft and drone exclusion
# areas (Table 6 of the Reef Authority's "Location-specific assessment"
# information sheet, 3 Dec 2024, http://hdl.handle.net/11017/3233). Drones
# must not be operated in these areas without specific Reef Authority
# permission. Aircraft must stay 1000 m laterally (or above 1500 ft), so each
# listed island, cay, rock or reef is buffered by 1000 m.
#
# The table is kept in data/gbrmpa_drone_exclusions.csv. Features are looked
# up by Reef ID (label_id) in GBRMPA's GBR Features layer, served by
# Geoscience Australia. No key is needed.
#
# Usage (from the project root):
#   Rscript scripts/build_gbrmpa_exclusions.R

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(sf)
})

features_url <- "https://services.ga.gov.au/gis/rest/services/AMSIS_Environment_Legacy/MapServer/8/query"
buffer_m <- 1000
table <- read.csv(file.path("data", "gbrmpa_drone_exclusions.csv"), stringsAsFactors = FALSE)

query_features <- function(where) {
  response <- GET(features_url, query = list(
    where = where, outFields = "label_id,gbr_name,feat_name,loc_name_s",
    returnGeometry = "true", outSR = 4326, f = "geojson"), timeout(120))
  stop_for_status(response, "query GBR features")
  text <- content(response, "text", encoding = "UTF-8")
  out <- tryCatch(st_read(text, quiet = TRUE), error = function(e) NULL)
  if (is.null(out) || !nrow(out)) return(NULL)
  st_make_valid(out)
}

# The place named in the table entry, e.g. "Bird Islands North (2) (...)" -> "Bird".
key_word <- function(description) {
  x <- sub("^(in the vicinity of|In the vicinity of|intertidal areas adjacent to)\\s+", "", description)
  x <- sub("^Frankland Islands Sector: ", "", x)
  x <- sub("^(un-named island immediately north of)\\s+", "", x)
  strsplit(trimws(x), "[ (,;]")[[1]][1]
}
name_pattern <- function(description) {
  x <- sub("^in the vicinity of\\s+", "", description)
  x <- sub("\\s*(Locality|\\().*$", "", x)
  x <- sub("^Low Island$", "Low Isl", x)
  trimws(x)
}

# Name search, limited to the same Reef ID latitude band (e.g. "22-") when
# the entry has a Reef ID. Prefers features carrying the full short name.
find_by_name <- function(description, reef_id) {
  word <- key_word(description)
  if (is.na(word) || nchar(word) < 3) return(NULL)
  where <- sprintf("UPPER(gbr_name) LIKE '%%%s%%'", toupper(gsub("'", "''", word)))
  if (nzchar(reef_id)) where <- sprintf("%s AND label_id LIKE '%s-%%'", where, substr(reef_id, 1, 2))
  found <- query_features(where)
  if (is.null(found)) return(NULL)
  # Compare without a trailing "Island(s)" ("Turtle Group Islands" is mapped
  # as "Turtle Group (No 1)" etc.).
  short <- sub("[[:space:]]+islands?$", "", tolower(name_pattern(description)))
  full <- grepl(short, tolower(found$gbr_name), fixed = TRUE)
  # Only the first word matched (e.g. "Little" for Little Armit Island): too
  # loose, so leave it to the Reef ID match.
  if (!any(full)) return(NULL)
  found <- found[full, ]
  # Same name elsewhere in the band (e.g. two "Rocky Islets" groups): keep
  # the features under this Reef ID when there are any.
  if (nzchar(reef_id)) {
    base <- toupper(sub("[a-z]$", "", reef_id))
    own <- startsWith(toupper(found$label_id), base)
    if (any(own)) found <- found[own, ]
  }
  if (any(found$feat_name != "Reef") && !grepl("Reef|Patch", description)) {
    found <- found[found$feat_name != "Reef", ]
  }
  found
}

rows <- list()
unmatched <- character()
for (i in seq_len(nrow(table))) {
  entry <- table[i, ]
  if (nzchar(entry$reef_id)) {
    found <- query_features(sprintf("UPPER(label_id) = '%s'", toupper(entry$reef_id)))
    match <- "Reef ID"
    word <- tolower(key_word(entry$description))
    is_reef <- grepl("Reef|Patch", entry$description)
    by_name <- if (is.null(found)) logical() else grepl(word, tolower(found$gbr_name), fixed = TRUE)
    if (!is.null(found) && is_reef) {
      # Reef entries cover the whole reef and its islands.
    } else if (any(by_name)) {
      found <- found[by_name, ]
      match <- "Reef ID and name"
    } else {
      # The Reef ID is missing or names a different feature: search by name.
      named <- find_by_name(entry$description, entry$reef_id)
      if (!is.null(named)) {
        found <- named
        match <- "name (Reef ID names a different feature)"
      } else if (!is.null(found) && any(found$feat_name != "Reef")) {
        found <- found[found$feat_name != "Reef", ]
        match <- "Reef ID (name differs in GBR Features)"
      }
    }
  } else {
    pattern <- name_pattern(entry$description)
    found <- query_features(sprintf("UPPER(gbr_name) LIKE '%%%s%%'", toupper(gsub("'", "''", pattern))))
    match <- "name"
  }
  if (is.null(found)) {
    unmatched <- c(unmatched, sprintf("%s %s", entry$reef_id, entry$description))
    next
  }
  centre <- st_coordinates(st_centroid(st_union(st_geometry(found))))[1, ]
  crs <- sprintf("+proj=aeqd +lat_0=%.8f +lon_0=%.8f +datum=WGS84 +units=m +no_defs",
                 centre[2], centre[1])
  zone <- st_union(st_buffer(st_transform(st_geometry(found), crs), buffer_m, nQuadSegs = 12))
  zone <- st_transform(st_simplify(zone, dTolerance = 20), 4326)
  label <- sub("^(in the vicinity of|In the vicinity of|intertidal areas adjacent to)\\s+", "",
               entry$description)
  rows[[length(rows) + 1L]] <- st_sf(
    category = "gbrmpa_exclusion",
    name = label,
    ident = entry$reef_id,
    lower_ft = 0,
    lower_ref = "GND",
    limits = entry$period,
    matched_by = match,
    matched_features = paste(unique(found$loc_name_s), collapse = "; "),
    geometry = st_cast(zone, "GEOMETRY")
  )
}
# --- Whole areas listed in Table 6 (not single features; no buffer) ---
# Hinchinbrook Island Dugong Protection Area (Hinchinbrook POM 2004): the two
# "Hinchinbrook Island Area" parts of the Species Conservation (Dugong
# Protection) Special Management Areas layer, GBRMPA open data.
sma_url <- paste0("https://services-ap1.arcgis.com/8gXWSCxaJlFIfiTr/arcgis/rest/services/",
                  "Great_Barrier_Reef_Marine_Park_Special_Management_Areas_20/FeatureServer/57/query")
response <- GET(sma_url, query = list(
  where = "LOC_NAME_L LIKE '%Hinchinbrook Island Area%' AND LOC_TYPE_L LIKE '%Dugong%'",
  outFields = "LOC_NAME_L,LOC_TYPE_L", returnGeometry = "true", outSR = 4326, f = "geojson"),
  timeout(120))
stop_for_status(response, "query GBRMPA Special Management Areas")
dpa <- st_read(content(response, "text", encoding = "UTF-8"), quiet = TRUE)
if (nrow(dpa) == 0) stop("Hinchinbrook Dugong Protection Area not found in the SMA layer.")
# The Marine Park SMA layer omits Queensland waters such as Hinchinbrook
# Channel. Queensland's Dugong Protection Area A ("Hinchinbrook Island area",
# Fisheries Regulation) covers them; it is republished as a public feature
# service by the Port of Townsville. Use the union of both outlines.
qld_url <- paste0("https://services-ap1.arcgis.com/KbOIDXPIMrJFkbq1/arcgis/rest/services/",
                  "Dugong_protection_areas/FeatureServer/0/query")
response <- GET(qld_url, query = list(
  where = "dpa_name = 'Hinchinbrook Island area'", outFields = "dpa_name,dpa_type",
  returnGeometry = "true", outSR = 4326, f = "geojson"), timeout(120))
stop_for_status(response, "query Queensland Dugong Protection Areas")
qld_dpa <- st_read(content(response, "text", encoding = "UTF-8"), quiet = TRUE)
if (nrow(qld_dpa) == 0) stop("Hinchinbrook Island area not found in the Queensland DPA layer.")
qld_dpa$LOC_NAME_L <- paste0("Queensland Dugong Protection Area ", qld_dpa$dpa_type, " - ", qld_dpa$dpa_name)
dpa <- rbind(dpa[, "LOC_NAME_L"], qld_dpa[, "LOC_NAME_L"])
rows[[length(rows) + 1L]] <- st_sf(
  category = "gbrmpa_exclusion",
  name = "Hinchinbrook Island Dugong Protection Area",
  ident = "",
  lower_ft = 0,
  lower_ref = "GND",
  limits = "All Year",
  matched_by = "GBRMPA Special Management Areas layer",
  matched_features = paste(dpa$LOC_NAME_L, collapse = "; "),
  # Simplify in metres; the source has duplicate vertices that spherical (s2)
  # geometry rejects.
  geometry = st_cast(st_transform(st_simplify(st_union(st_make_valid(st_transform(
    st_geometry(dpa), 3577))), dTolerance = 10), 4326), "GEOMETRY")
)
message(sprintf("Added Hinchinbrook Island Dugong Protection Area (%d parts).", nrow(dpa)))

layer <- do.call(rbind, rows)
out <- file.path("data", "gbrmpa_exclusions.geojson")
unlink(out)
st_write(layer, out, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=6", "RFC7946=YES"))
message(sprintf("Wrote %d GBRMPA exclusion areas; %d table entries not found:", nrow(layer), length(unmatched)))
if (length(unmatched)) message(paste(" -", unmatched, collapse = "\n"))
mismatch <- layer[layer$matched_by != "Reef ID and name" & layer$matched_by != "name", ]
if (nrow(mismatch)) {
  message("Check these (matched by Reef ID only):")
  message(paste(" -", mismatch$ident, mismatch$name, "->", mismatch$matched_features, collapse = "\n"))
}
