helper_path <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "planning_tools.R"),
  mustWork = TRUE
)
repo_root <- dirname(helper_path)
source(helper_path, local = FALSE)

write_restriction_geojson <- function(features_json) {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    sprintf('{"type":"FeatureCollection","features":[%s]}', paste(features_json, collapse = ",")),
    path
  )
  path
}
polygon_feature <- function(coords, name_json = '{"name":"Zone A"}') {
  ring <- paste(vapply(coords, function(pt) sprintf("[%s,%s]", pt[1], pt[2]), character(1)), collapse = ",")
  sprintf('{"type":"Feature","properties":%s,"geometry":{"type":"Polygon","coordinates":[[%s]]}}', name_json, ring)
}
square <- function(xmin, ymin, xmax, ymax) {
  list(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin))
}

test_that("helper copies stay identical across editions", {
  root_copy <- normalizePath(file.path(repo_root, "..", "planning_tools.R"), mustWork = TRUE)
  expect_identical(
    readLines(helper_path, warn = FALSE),
    readLines(root_copy, warn = FALSE)
  )
})

test_that("a GeoJSON polygon imports with its name", {
  path <- write_restriction_geojson(c(polygon_feature(square(146.8, -19.3, 146.9, -19.2))))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  expect_equal(nrow(zones), 1L)
  expect_equal(zones$zone_name, "Zone A")
  expect_true(grepl("restriction", restriction_group, ignore.case = TRUE))
})

test_that("missing names fall back to numbered zones", {
  path <- write_restriction_geojson(c(polygon_feature(square(146.8, -19.3, 146.9, -19.2), "{}")))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  expect_equal(zones$zone_name, "Restriction zone 1")
})

test_that("unsafe label HTML is neutralised at render time", {
  evil <- '{"name":"<script>alert(1)</script>"}'
  path <- write_restriction_geojson(c(polygon_feature(square(146.8, -19.3, 146.9, -19.2), evil)))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  escaped <- as.character(htmltools::htmlEscape(zones$zone_name))
  expect_false(grepl("<script>", escaped, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", escaped, fixed = TRUE))
})

test_that("mixed polygons and points import polygons and report the rest", {
  point <- '{"type":"Feature","properties":{},"geometry":{"type":"Point","coordinates":[146.85,-19.25]}}'
  path <- write_restriction_geojson(c(
    polygon_feature(square(146.8, -19.3, 146.9, -19.2)),
    point
  ))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  expect_equal(nrow(zones), 1L)
  expect_equal(attr(zones, "ignored_non_polygon"), 1L)
})

test_that("a file with no polygons is rejected", {
  point <- '{"type":"Feature","properties":{},"geometry":{"type":"Point","coordinates":[146.85,-19.25]}}'
  path <- write_restriction_geojson(c(point))
  on.exit(unlink(path), add = TRUE)
  expect_error(read_restriction_zones(path, "zones.geojson"), "No polygons")
})

test_that("multipolygons and holes import", {
  multi <- '{"type":"Feature","properties":{"name":"Multi"},"geometry":{"type":"MultiPolygon","coordinates":[[[[146.8,-19.2],[146.85,-19.2],[146.85,-19.25],[146.8,-19.25],[146.8,-19.2]]],[[[146.9,-19.3],[146.95,-19.3],[146.95,-19.35],[146.9,-19.35],[146.9,-19.3]]]]}}'
  hole <- '{"type":"Feature","properties":{"name":"Holed"},"geometry":{"type":"Polygon","coordinates":[[[146.8,-19.2],[146.9,-19.2],[146.9,-19.3],[146.8,-19.3],[146.8,-19.2]],[[146.82,-19.22],[146.84,-19.22],[146.84,-19.24],[146.82,-19.24],[146.82,-19.22]]]}}'
  path <- write_restriction_geojson(c(multi, hole))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  expect_equal(nrow(zones), 2L)
  expect_true(all(as.character(sf::st_geometry_type(zones)) %in% c("POLYGON", "MULTIPOLYGON")))
})

test_that("KML polygons import and KML points do not fail the file", {
  kml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2"><Document>',
    '<Placemark><name>Strip</name><Polygon><outerBoundaryIs><LinearRing><coordinates>',
    '146.8,-19.2,0 146.9,-19.2,0 146.9,-19.3,0 146.8,-19.3,0 146.8,-19.2,0',
    '</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>',
    '<Placemark><name>Beacon</name><Point><coordinates>146.85,-19.25,0</coordinates></Point></Placemark>',
    '</Document></kml>'
  )
  path <- tempfile(fileext = ".kml")
  writeLines(kml, path)
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.kml")
  expect_equal(nrow(zones), 1L)
  expect_equal(zones$zone_name, "Strip")
  expect_equal(attr(zones, "ignored_non_polygon"), 1L)
})

test_that("invalid and empty geometries are rejected", {
  bowtie <- '{"type":"Feature","properties":{"name":"Bad"},"geometry":{"type":"Polygon","coordinates":[[[146.8,-19.2],[146.9,-19.3],[146.9,-19.2],[146.8,-19.3],[146.8,-19.2]]]}}'
  path <- write_restriction_geojson(c(bowtie))
  on.exit(unlink(path), add = TRUE)
  expect_error(read_restriction_zones(path, "zones.geojson"), "invalid")

  empty_path <- write_restriction_geojson(character())
  on.exit(unlink(empty_path), add = TRUE)
  expect_error(read_restriction_zones(empty_path, "empty.geojson"), "no restriction|no polygons", ignore.case = TRUE)
})

test_that("unsupported extensions are rejected", {
  path <- tempfile(fileext = ".txt")
  writeLines("not spatial", path)
  on.exit(unlink(path), add = TRUE)
  expect_error(read_restriction_zones(path, "zones.txt"), "GeoJSON or KML")
})

test_that("overlap distinguishes disjoint, contained, crossing, and boundary touch", {
  path <- write_restriction_geojson(c(polygon_feature(square(146.8, -19.3, 146.9, -19.2))))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")

  far <- data.frame(lng = c(147.5, 147.6, 147.6, 147.5), lat = c(-19.2, -19.2, -19.3, -19.3))
  inside <- data.frame(lng = c(146.82, 146.88, 146.88, 146.82), lat = c(-19.22, -19.22, -19.28, -19.28))
  crossing <- data.frame(lng = c(146.85, 146.95, 146.95, 146.85), lat = c(-19.22, -19.22, -19.28, -19.28))
  touching <- data.frame(lng = c(146.7, 146.8, 146.8, 146.7), lat = c(-19.22, -19.22, -19.28, -19.28))

  expect_equal(restriction_overlap_count(far, zones), 0L)
  expect_equal(restriction_overlap_count(inside, zones), 1L)
  expect_equal(restriction_overlap_count(crossing, zones), 1L)
  # Conservative: shared boundary counts as an intersection worth checking.
  expect_equal(restriction_overlap_count(touching, zones), 1L)
})

test_that("an AOI fully inside a hole does not count as overlap", {
  hole <- '{"type":"Feature","properties":{"name":"Holed"},"geometry":{"type":"Polygon","coordinates":[[[146.8,-19.2],[146.9,-19.2],[146.9,-19.3],[146.8,-19.3],[146.8,-19.2]],[[146.82,-19.22],[146.84,-19.22],[146.84,-19.24],[146.82,-19.24],[146.82,-19.22]]]}}'
  path <- write_restriction_geojson(c(hole))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  in_hole <- data.frame(lng = c(146.825, 146.835, 146.835, 146.825), lat = c(-19.225, -19.225, -19.235, -19.235))
  expect_equal(restriction_overlap_count(in_hole, zones), 0L)
})

test_that("missing inputs yield zero and failed checks yield unavailable", {
  path <- write_restriction_geojson(c(polygon_feature(square(146.8, -19.3, 146.9, -19.2))))
  on.exit(unlink(path), add = TRUE)
  zones <- read_restriction_zones(path, "zones.geojson")
  expect_equal(restriction_overlap_count(NULL, zones), 0L)
  expect_equal(restriction_overlap_count(data.frame(lng = c(146.82, 146.88, 146.88), lat = c(-19.22, -19.22, -19.28)), NULL), 0L)
  expect_true(is.na(restriction_overlap_count(data.frame(lng = c(146.8, 146.85), lat = c(-19.2, -19.25)), zones)))
  bad <- data.frame(lng = c(1, 2), lat = c(1, 2))
  expect_true(is.na(restriction_overlap_count(bad, zones)))
})

test_that("both apps wire measurement, overlay toggle, and restriction server", {
  for (app_file in c(
    file.path(repo_root, "..", "app.R"),
    file.path(repo_root, "app.R")
  )) {
    lines <- readLines(app_file, warn = FALSE)
    text <- paste(lines, collapse = "\n")
    expect_true(grepl("addMeasure", text, fixed = TRUE), info = app_file)
    expect_true(grepl('primaryLengthUnit = "meters"', text, fixed = TRUE), info = app_file)
    expect_true(grepl('secondaryLengthUnit = "kilometers"', text, fixed = TRUE), info = app_file)
    expect_true(grepl("overlayGroups", text, fixed = TRUE), info = app_file)
    expect_true(grepl("restriction_tools_server", text, fixed = TRUE), info = app_file)
    expect_true(grepl("restriction_tools_ui", text, fixed = TRUE), info = app_file)
  }
})
