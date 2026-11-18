# Land exclusion for marine surveys: removes the parts of transects that lie on
# land, using the DEA-derived Australian land mask in data/land_mask (GDA94 /
# Australian Albers, metres). Unlike no-fly zones, land may be crossed in
# transit, so only survey lines are clipped.

land_mask_path <- file.path("data", "land_mask", "australia_land_mask_from_dea.shp")

load_land_mask <- function(path = land_mask_path) {
  land <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(error) NULL)
  if (is.null(land) || nrow(land) == 0) return(NULL)
  land <- sf::st_geometry(land)
  bad <- !sf::st_is_valid(land)
  if (any(bad)) land[bad] <- sf::st_make_valid(land[bad])
  land
}

# Returns transects with land removed; pieces shorter than min_length_m are
# dropped. Carries attr "land_clip" = list(affected, removed_m).
clip_transects_off_land <- function(land, transects, min_length_m = 10) {
  info <- list(affected = 0L, removed_m = 0)
  done <- function(x) structure(x, land_clip = info)
  if (is.null(land) || !length(transects)) return(done(transects))
  crs <- sf::st_crs(land)
  lines <- sf::st_transform(sf::st_sfc(lapply(transects, function(m) {
    sf::st_linestring(as.matrix(m)[, 1:2, drop = FALSE])
  }), crs = 4326), crs)
  box <- sf::st_as_sfc(sf::st_bbox(lines))
  candidates <- land[lengths(sf::st_intersects(land, box)) > 0]
  if (!length(candidates)) return(done(transects))
  # Cut the land (often the whole mainland polygon) down to a box just around
  # the survey before the overlay; this is the slow step otherwise.
  window <- sf::st_bbox(sf::st_buffer(box, 200))
  land_here <- sf::st_union(suppressWarnings(sf::st_crop(candidates, window)))

  cut <- cut_lines_outside(lines, land_here, transects, min_length_m)
  info$affected <- cut$affected
  info$removed_m <- cut$removed_m
  result <- cut$transects
  done(result)
}
