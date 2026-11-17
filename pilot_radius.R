# An ellipsoidal azimuthal equidistant projection preserves distances from
# the pilot. Use an inscribed polygon with a small margin for transformations.
pilot_radius_m <- 900

pilot_radius_geometry <- function(pilot, radius_m = pilot_radius_m) {
  stopifnot(length(pilot) == 2L, all(is.finite(pilot)),
            abs(pilot[1]) <= 180, abs(pilot[2]) < 90,
            is.finite(radius_m), radius_m > 0.05)
  crs <- sprintf(
    "+proj=aeqd +lat_0=%.12f +lon_0=%.12f +datum=WGS84 +units=m +no_defs",
    pilot[2], pilot[1]
  )
  origin <- sf::st_sfc(sf::st_point(c(0, 0)), crs = crs)
  sf::st_buffer(origin, radius_m - 0.05, nQuadSegs = 180)
}

clip_transects_to_pilot_radius <- function(transects, pilot,
                                          radius_m = pilot_radius_m) {
  boundary <- pilot_radius_geometry(pilot, radius_m)
  result <- list()
  for (coords in transects) {
    line <- sf::st_sfc(sf::st_linestring(coords[, 1:2, drop = FALSE]), crs = 4326)
    clipped <- suppressWarnings(sf::st_intersection(
      sf::st_transform(line, sf::st_crs(boundary)), boundary
    ))
    if (length(clipped) == 0L || all(sf::st_is_empty(clipped))) next
    # Tangencies can yield a point; only retain survey line segments.
    clipped <- clipped[sf::st_geometry_type(clipped) %in%
      c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")]
    if (length(clipped) == 0L) next
    lines <- suppressWarnings(sf::st_collection_extract(clipped, "LINESTRING"))
    lines <- suppressWarnings(sf::st_cast(lines, "LINESTRING"))
    for (index in seq_along(lines)) {
      if (sf::st_is_empty(lines[index]) || as.numeric(sf::st_length(lines[index])) < 0.01) next
      result[[length(result) + 1L]] <- sf::st_coordinates(
        sf::st_transform(lines[index], 4326)
      )[, 1:2, drop = FALSE]
    }
  }
  result
}
