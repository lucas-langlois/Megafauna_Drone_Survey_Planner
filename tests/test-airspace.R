library(testthat)
project_root <- normalizePath(file.path(testthat::test_path(), ".."))
source(file.path(project_root, "airspace_check.R"))
layer <- load_airspace_layer(file.path(project_root, airspace_data_path),
                             file.path(project_root, airspace_meta_path))

square <- function(lon, lat, half = 0.002) {
  rbind(c(lon - half, lat - half), c(lon + half, lat - half),
        c(lon + half, lat + half), c(lon - half, lat + half))
}
categories_at <- function(lon, lat, altitude_m = 91.4, half = 0.002) {
  check_airspace(layer, square(lon, lat, half), altitude_m = altitude_m)$findings$category
}

test_that("bundled layer holds only rule-relevant categories", {
  expect_false(is.null(layer))
  expect_true(all(layer$category %in% names(airspace_severity)))
  controlled <- layer$ident[layer$category == "controlled_aerodrome"]
  # Includes towered aerodromes without a control zone of their own (YMEN).
  expect_true(all(c("YBTL", "YBCS", "YBMK", "YMEN", "YSSY") %in% controlled))
  # Coastal band only: deep-inland aerodromes are dropped.
  expect_false(any(c("YBAS", "YSCB", "YPTN") %in% layer$ident))
  expect_match(airspace_data_label(layer), "airspace updated \\d{4}-\\d{2}-\\d{2}")
})

test_that("islands are kept", {
  for (ident in c("YPAM", "YBHM", "YLHI", "YKII", "YFLI", "YMTI")) {
    expect_true(ident %in% layer$ident, info = ident)
  }
})

test_that("Bowen gets a DJI zone, a 3 NM caution zone and runway paths", {
  bowen <- layer[layer$ident == "YBWN", ]
  expect_setequal(bowen$category, c("aerodrome", "aerodrome_approach"))
  # Airport itself: DJI restricted circle is the most restrictive finding.
  at_airport <- check_airspace(layer, square(148.2138, -20.0193), altitude_m = 60)$findings
  expect_equal(at_airport$severity[1], "dji-block")
  expect_true(all(c("aerodrome", "aerodrome_approach") %in% at_airport$category))
  # 5 km out along runway 04/22's extended centreline (towards 224 degrees):
  # inside the approach path, outside the DJI circle.
  probe <- geosphere::destPoint(c(148.2089, -20.0233), 224, 5000)
  path <- check_airspace(layer, square(probe[1], probe[2], 0.0003), altitude_m = 60)$findings
  expect_true("aerodrome_approach" %in% path$category)
  expect_false(any(path$severity == "dji-block"))
  # Approach paths are no-fly at every aerodrome; the 3 NM zone is a caution.
  expect_equal(path$severity[path$category == "aerodrome_approach"], "no-fly")
  expect_true(all(path$severity[path$category == "aerodrome"] == "caution"))
})

test_that("controlled aerodrome zone and paths are no-fly", {
  result <- check_airspace(layer, square(146.77, -19.26), altitude_m = 91.4)
  expect_equal(result$status, "checked")
  expect_true(all(c("dji_restricted", "controlled_aerodrome", "controlled_approach") %in%
                    result$findings$category))
  expect_true(all(result$findings$severity[result$findings$category == "controlled_aerodrome"] == "no-fly"))
})

test_that("3 NM zone is measured from the runway centreline", {
  # Westernmost Townsville threshold (runway 07); 5.50 km due west is inside,
  # 5.65 km is not.
  inside <- geosphere::destPoint(c(146.7560, -19.2558), 270, 5500)
  outside <- geosphere::destPoint(c(146.7560, -19.2558), 270, 5650)
  expect_true("controlled_aerodrome" %in% categories_at(inside[1], inside[2], half = 0.0001))
  expect_false("controlled_aerodrome" %in% categories_at(outside[1], outside[2], half = 0.0001))
})

test_that("outer approach paths apply only at or above 300 ft", {
  upper <- layer[layer$category == "controlled_approach_upper" & layer$ident == "YBTL", ]
  point <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(upper)[1])))
  low <- categories_at(point[1], point[2], altitude_m = 60, half = 0.0001)
  high <- categories_at(point[1], point[2], altitude_m = 100, half = 0.0001)
  expect_false("controlled_approach_upper" %in% low)
  expect_true("controlled_approach_upper" %in% high)
})

test_that("ground-level restricted areas (Rattlesnake Island) are no-fly findings", {
  result <- check_airspace(layer, square(146.611, -19.037), altitude_m = 60)$findings
  expect_true(any(grepl("R747", result$name) & result$severity == "no-fly"))
})

test_that("route connectors outside the AOI are checked", {
  aoi <- square(148.60, -19.70)
  expect_equal(nrow(check_airspace(layer, aoi)$findings), 0L)
  # A route leg that strays into the Townsville zones must be reported.
  route <- list(rbind(c(148.60, -19.70), c(148.61, -19.70)),
                rbind(c(146.77, -19.26), c(146.78, -19.26)))
  findings <- check_airspace(layer, aoi, route)$findings
  expect_true(any(findings$category == "controlled_aerodrome"))
})

test_that("an empty result is never worded as clear", {
  text <- airspace_summary_text(check_airspace(layer, square(148.60, -19.70)))
  expect_match(text, "No intersections found")
  expect_match(text, "Advisory only")
  expect_false(grepl("\\bclear\\b", text, ignore.case = TRUE))
})

test_that("missing data reports the check as not performed", {
  expect_null(load_airspace_layer(tempfile(fileext = ".geojson")))
  result <- check_airspace(NULL, square(146.77, -19.26))
  expect_equal(result$status, "not_performed")
  expect_match(airspace_summary_text(result), "not performed")
})

test_that("clipping removes the no-fly parts of transects but keeps caution areas", {
  # East-west line from Townsville airport out across Cleveland Bay.
  line <- rbind(c(146.77, -19.20), c(146.90, -19.20))
  clipped <- clip_transects_outside_airspace(layer, list(line), altitude_m = 91.4)
  info <- attr(clipped, "airspace_clip")
  expect_equal(info$affected, 1L)
  expect_gt(info$removed_m, 1000)
  expect_gte(length(clipped), 1L)
  # Nothing left over may intersect a no-fly or DJI zone.
  forbidden <- airspace_forbidden_union(layer, c(146.7, -19.3, 147.0, -19.1), 91.4)
  for (part in clipped) {
    piece <- sf::st_sfc(sf::st_linestring(part), crs = 4326)
    # Only boundary contact (within 1 m) is allowed.
    inside <- suppressWarnings(sf::st_intersection(piece, forbidden))
    expect_lt(sum(as.numeric(sf::st_length(inside))), 1)
  }
  # Caution-only lines (Bowen's 3 NM zone outside its runway paths) are untouched.
  bowen <- layer[layer$ident == "YBWN", ]
  local <- "+proj=aeqd +lat_0=-20.02 +lon_0=148.21 +datum=WGS84 +units=m +no_defs"
  near_bowen <- sf::st_transform(layer[layer$ident == "YBWN" | layer$category == "dji_restricted", ], local)
  near_bowen <- near_bowen[as.numeric(sf::st_distance(near_bowen, near_bowen[1, ])) < 20000, ]
  caution_only <- sf::st_difference(
    sf::st_geometry(near_bowen[near_bowen$category == "aerodrome", ]),
    sf::st_union(sf::st_buffer(sf::st_geometry(near_bowen[near_bowen$category %in%
      c("aerodrome_approach", "dji_restricted"), ]), 200)))
  probe <- suppressWarnings(sf::st_coordinates(sf::st_transform(
    sf::st_point_on_surface(caution_only), 4326)))[1, 1:2]
  caution_line <- rbind(probe - c(0.0005, 0), probe + c(0.0005, 0))
  kept <- clip_transects_outside_airspace(layer, list(caution_line), altitude_m = 60)
  expect_equal(attr(kept, "airspace_clip")$affected, 0L)
  expect_equal(kept[[1]], caution_line)
})

test_that("route-only check ignores the AOI and boundary contact", {
  aoi <- square(146.80, -19.20, half = 0.02)
  line <- rbind(c(146.78, -19.20), c(146.82, -19.20))
  clipped <- clip_transects_outside_airspace(layer, list(line), altitude_m = 91.4)
  full <- check_airspace(layer, aoi, clipped, altitude_m = 91.4)
  route_only <- check_airspace(layer, aoi, clipped, altitude_m = 91.4, include_aoi = FALSE)
  expect_true(any(full$findings$severity == "no-fly"))
  expect_false(any(route_only$findings$severity %in% c("no-fly", "dji-block")))
})

test_that("routing keeps every clipped piece and never crosses a no-fly zone", {
  xs <- seq(146.8055, 146.8445, length.out = 33)
  lines <- lapply(xs, function(x) rbind(c(x, -19.1745), c(x, -19.2155)))
  clipped <- clip_transects_outside_airspace(layer, lines, 90)
  ordered <- clipped[order(-vapply(clipped, function(m) m[1, 1], 0),
                           -vapply(clipped, function(m) max(m[, 2]), 0))]
  ordered <- lapply(seq_along(ordered), function(i) {
    m <- ordered[[i]]
    m[order(if (i %% 2) -m[, 2] else m[, 2]), , drop = FALSE]
  })
  routed <- route_around_zones(layer, ordered, 90)
  expect_equal(length(routed), length(clipped))
  expect_gt(attr(routed, "airspace_route")$blocks, 1L)
  aoi <- rbind(c(146.80, -19.17), c(146.85, -19.17), c(146.85, -19.22), c(146.80, -19.22))
  findings <- check_airspace(layer, aoi, routed, 90, include_aoi = FALSE)$findings
  expect_false(any(findings$severity %in% c("no-fly", "dji-block")))
})

test_that("the 300 ft outer approach paths apply only above 91 m", {
  expect_false(airspace_zone_applies(300, 91))
  expect_true(airspace_zone_applies(300, 91.4))
  expect_true(airspace_zone_applies(0, 10))
})

test_that("areas split by a runway path become separate missions with no crossing", {
  xs <- seq(148.2385, 148.2675, length.out = 25)
  lines <- lapply(xs, function(x) rbind(c(x, -20.0295), c(x, -20.0545)))
  clipped <- clip_transects_outside_airspace(layer, lines, 90)
  ordered <- clipped[order(-vapply(clipped, function(m) m[1, 1], 0),
                           -vapply(clipped, function(m) max(m[, 2]), 0))]
  routed <- route_around_zones(layer, ordered, 90)
  info <- attr(routed, "airspace_route")
  expect_gte(info$missions, 2L)
  expect_equal(length(info$mission), length(routed))
  aoi <- rbind(c(148.238, -20.029), c(148.268, -20.029), c(148.268, -20.055), c(148.238, -20.055))
  findings <- check_airspace(layer, aoi, routed, 90, include_aoi = FALSE)$findings
  expect_false(any(findings$severity %in% c("no-fly", "dji-block")))
})

test_that("complete plan exports one DJI and Litchi file per mission", {
  app_env <- new.env(parent = globalenv())
  sys.source(file.path(project_root, "app.R"), envir = app_env, chdir = TRUE)
  old <- setwd(project_root)
  on.exit(setwd(old), add = TRUE)
  shiny::testServer(app_env$server, {
    session$setInputs(altitude = 90, front_overlap = 70, transect_gap_m = 0,
      drone_model = "m3e", transect_angle = 0, start_corner = "top_right",
      reverse_transect_order = FALSE, trim_transect_length = FALSE,
      show_transects = FALSE, show_photopoints = FALSE, show_footprints = FALSE,
      pilot_distance_m = 900, limit_pilot_radius = FALSE, avoid_no_fly_zones = TRUE,
      export_name = "")
    drawn_polygon(data.frame(lng = c(148.238, 148.268, 148.268, 148.238, 148.238),
                             lat = c(-20.055, -20.055, -20.029, -20.029, -20.055)))
    session$flushReact()
    plan <- build_plan_summary_data()
    expect_gte(plan$metrics$mission_count, 2L)
    expect_equal(plan$metrics$airspace_no_fly_count, 0L)
    archive <- tempfile(fileext = ".zip")
    files <- write_complete_plan(archive)
    listed <- zip::zip_list(archive)$filename
    expect_equal(sum(grepl("DJI_Mission_[0-9]+.*[.]kmz$", listed)), plan$metrics$mission_count)
    expect_equal(sum(grepl("Litchi_Bundle_Mission_[0-9]+.*[.]zip$", listed)), plan$metrics$mission_count)
    # Inner Litchi files carry their mission number, so bundles extracted into
    # one folder never overwrite each other.
    inner <- unlist(lapply(grep("Litchi_Bundle_Mission_", listed, value = TRUE), function(b) {
      d <- tempfile(); zip::unzip(archive, files = b, exdir = d)
      zip::zip_list(file.path(d, b))$filename
    }))
    expect_false(anyDuplicated(inner) > 0)
    expect_true(all(grepl("_Mission_[0-9]+", inner)))
    readme <- tempfile()
    zip::unzip(archive, files = "README.txt", exdir = readme)
    expect_match(paste(readLines(file.path(readme, "README.txt")), collapse = "\n"), "SEPARATE MISSIONS")
  })
})

test_that("GBRMPA drone exclusion areas are loaded and treated as no-fly", {
  gbrmpa <- layer[layer$category == "gbrmpa_exclusion", ]
  expect_gt(nrow(gbrmpa), 100L)
  expect_true(all(c("23-052", "14-118", "18-008a") %in% gbrmpa$ident))
  # Heron Island (23-052): the island itself and 800 m off it are excluded.
  heron <- check_airspace(layer, square(151.914, -23.442, half = 0.0005), altitude_m = 60)$findings
  expect_true(any(heron$category == "gbrmpa_exclusion" & heron$severity == "no-fly"))
  expect_match(heron$detail[heron$category == "gbrmpa_exclusion"][1], "Reef Authority permission")
  clipped <- clip_transects_outside_airspace(layer,
    list(rbind(c(151.88, -23.442), c(151.95, -23.442))), altitude_m = 60)
  expect_equal(attr(clipped, "airspace_clip")$affected, 1L)
})

test_that("the Hinchinbrook Island Dugong Protection Area is a GBRMPA exclusion", {
  dpa <- layer[layer$name == "Hinchinbrook Island Dugong Protection Area", ]
  expect_equal(nrow(dpa), 1L)
  expect_equal(dpa$category, "gbrmpa_exclusion")
  # Missionary Bay, north of Hinchinbrook Island.
  point <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(dpa))))[1, ]
  findings <- check_airspace(layer, square(point[1], point[2], half = 0.0005), altitude_m = 60)$findings
  expect_true(any(grepl("Dugong Protection Area", findings$name) & findings$severity == "no-fly"))
})

test_that("the Hinchinbrook Dugong Protection Area covers Hinchinbrook Channel", {
  channel <- check_airspace(layer, square(146.10, -18.33, half = 0.001), altitude_m = 60)$findings
  expect_true(any(grepl("Dugong Protection Area", channel$name)))
})

test_that("the DEA land mask removes survey lines over land, including islands", {
  source(file.path(project_root, "land_mask.R"))
  land <- load_land_mask(file.path(project_root, land_mask_path))
  expect_false(is.null(land))
  # West-east line from Townsville's Strand out to sea: the land part is cut.
  coast <- list(rbind(c(146.78, -19.255), c(146.86, -19.255)))
  clipped <- clip_transects_off_land(land, coast)
  expect_equal(attr(clipped, "land_clip")$affected, 1L)
  expect_gt(attr(clipped, "land_clip")$removed_m, 100)
  # A line across Magnetic Island keeps only the water on both sides.
  island <- list(rbind(c(146.78, -19.14), c(146.90, -19.14)))
  split <- clip_transects_off_land(land, island)
  expect_gte(length(split), 2L)
  # Open water is untouched.
  sea <- list(rbind(c(147.20, -19.00), c(147.25, -19.00)))
  kept <- clip_transects_off_land(land, sea)
  expect_equal(attr(kept, "land_clip")$affected, 0L)
  expect_equal(kept[[1]], sea[[1]])
})

test_that("lines wholly inside a zone are counted and pieces stay with their line", {
  crs <- sf::st_crs("+proj=aeqd +lat_0=-20 +lon_0=148 +datum=WGS84 +units=m +no_defs")
  zone <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(300, 0), c(300, 300), c(0, 300), c(0, 0)))), crs = crs)
  local <- list(
    rbind(c(100, 100), c(200, 200)),   # wholly inside: removed
    rbind(c(-100, 150), c(400, 150)),  # crosses: two pieces of 100 m
    rbind(c(-100, 500), c(400, 500))   # clear of the zone
  )
  lines <- sf::st_sfc(lapply(local, sf::st_linestring), crs = crs)
  lonlat <- lapply(local, function(m) sf::sf_project(crs$wkt, "EPSG:4326", m))
  cut <- cut_lines_outside(lines, zone, lonlat, min_length_m = 10)
  expect_equal(cut$affected, 2L)
  expect_equal(cut$removed_m, sqrt(2) * 100 + 300, tolerance = 1e-6)
  expect_length(cut$transects, 3L)
  back <- lapply(cut$transects[1:2], function(m) sf::sf_project("EPSG:4326", crs$wkt, m))
  expect_equal(sort(c(back[[1]][, 1], back[[2]][, 1])), c(-100, 0, 300, 400), tolerance = 1e-6)
  expect_equal(cut$transects[[3]], lonlat[[3]])
})

test_that("detour legs in front of a survey line are flown without photos", {
  app_env <- new.env(parent = globalenv())
  sys.source(file.path(project_root, "app.R"), envir = app_env, chdir = TRUE)
  old <- setwd(project_root)
  on.exit(setwd(old), add = TRUE)
  shiny::testServer(app_env$server, {
    session$setInputs(altitude = 60, front_overlap = 80, transect_gap_m = 0,
      drone_model = "m3e", transect_angle = 0, start_corner = "top_right",
      reverse_transect_order = FALSE, trim_transect_length = FALSE,
      show_transects = FALSE, show_photopoints = FALSE, show_footprints = FALSE,
      pilot_distance_m = 900, limit_pilot_radius = FALSE, export_name = "")
    drawn_polygon(data.frame(lng = c(146.80, 146.85, 146.85, 146.80),
                             lat = c(-19.22, -19.22, -19.17, -19.17)))
    session$flushReact()
    res <- generate_transects_and_photopoints(drawn_polygon(), 60, 0, camera_params(), 0, FALSE)
    route <- order_route(res$transects, "top_right", 0)
    k <- which(vapply(route, nrow, 0L) > 2)[1]
    expect_false(is.na(k))
    m <- route[[k]]
    lead <- app_env$transect_lead_in(m)
    expect_gt(lead, 0L)
    survey_m <- geosphere::distGeo(m[nrow(m) - 1, ], m[nrow(m), ])
    spacing <- capture_settings()$actual_spacing_m
    # Photo centres only along the survey segment.
    pp <- app_env$generate_photo_points_for_transects(route[k], spacing)[[1]]
    expect_equal(nrow(pp), floor(survey_m / spacing) + 1)
    expect_equal(attr(pp, "bearing"),
      geosphere::bearing(m[nrow(m) - 1, ], m[nrow(m), ]) %% 360, tolerance = 1e-9)
    # DJI interval photos start at the survey segment, not the detour.
    rec <- app_env$build_dji_waypoint_records(route)
    first_wp <- sum(vapply(route[seq_len(k - 1)], nrow, 0L))
    expect_equal(unname(rec$transect_ranges[[k]][["start"]]), first_wp + lead)
    # Litchi: no photo interval on the detour rows.
    lp <- litchi_native_plan_for()
    rows <- first_wp + seq_len(nrow(m))
    expect_equal(lp$photo_timeinterval[rows[seq_len(lead)]], rep(0, lead))
    expect_gt(lp$photo_timeinterval[rows[lead + 1]], 0)
    # Survey distance excludes the detour.
    measured <- app_env$measure_transect_route(route[k])
    expect_equal(measured$survey_distance_m, survey_m, tolerance = 1)
  })
})
