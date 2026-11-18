library(testthat)
project_root <- normalizePath(file.path(testthat::test_path(), ".."))
source(file.path(project_root, "pilot_radius.R"))

local_line <- function(pilot, coords) {
  boundary <- pilot_radius_geometry(pilot)
  sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_linestring(coords), crs = sf::st_crs(boundary)), 4326
  ))[, 1:2, drop = FALSE]
}

expect_within_radius <- function(lines, pilot, radius) {
  for (line in lines) {
    # Check the flight legs between waypoints, as well as the endpoints.
    for (i in seq_len(nrow(line) - 1L)) {
      points <- geosphere::gcIntermediate(line[i, ], line[i + 1L, ],
        n = 50, addStartEnd = TRUE)
      expect_lte(max(geosphere::distGeo(pilot, points)), radius)
    }
  }
}

test_that("crossing lines are shortened, outside lines removed, inside lines retained", {
  for (pilot in list(c(146.8169, -19.259), c(-73, 45))) {
    crossing <- local_line(pilot, rbind(c(-1500, 0), c(1500, 0)))
    inside <- local_line(pilot, rbind(c(-100, 100), c(100, 100)))
    outside <- local_line(pilot, rbind(c(-1200, 1100), c(1200, 1100)))
    clipped <- clip_transects_to_pilot_radius(list(crossing, inside, outside), pilot)
    expect_length(clipped, 2L)
    expect_equal(clipped[[2]], inside, tolerance = 1e-7)
    expect_equal(as.numeric(geosphere::distGeo(clipped[[1]][1, ], clipped[[1]][2, ])),
      1800, tolerance = 0.2)
    expect_within_radius(clipped, pilot, 900)
    # Straight connectors between retained transects must also remain inside.
    expect_within_radius(list(rbind(clipped[[1]][2, ], clipped[[2]][1, ])), pilot, 900)
  }
})

test_that("custom distances, empty inputs and tangencies are handled", {
  pilot <- c(146.8169, -19.259)
  line <- local_line(pilot, rbind(c(-2000, 0), c(2000, 0)))
  for (radius in c(1, 250, 1000)) {
    result <- clip_transects_to_pilot_radius(list(line), pilot, radius)
    expect_length(result, 1L)
    expect_within_radius(result, pilot, radius)
  }
  expect_length(clip_transects_to_pilot_radius(list(), pilot), 0L)
  tangent <- local_line(pilot, rbind(c(-1000, 900), c(1000, 900)))
  expect_length(clip_transects_to_pilot_radius(list(tangent), pilot), 0L)
  expect_error(pilot_radius_geometry(pilot, 0))
})

app_env <- new.env(parent = globalenv())
sys.source(file.path(project_root, "app.R"), envir = app_env, chdir = TRUE)

test_that("custom export names are safe for local filenames", {
  expect_equal(app_env$safe_export_name(NULL), "")
  expect_equal(app_env$safe_export_name("   "), "")
  expect_equal(app_env$safe_export_name("Bowen Site 01"), "Bowen_Site_01")
  expect_false(grepl('[<>:"/|?*]', app_env$safe_export_name('Bowen/2026:*?')))
  expect_lte(nchar(app_env$safe_export_name(paste(rep("a", 100), collapse = ""))), 80)
})

test_that("placement and radius changes propagate to route, photos, metrics and exports", {
  shiny::testServer(app_env$server, {
    session$setInputs(altitude = 91.4, front_overlap = 70, transect_gap_m = 0,
      drone_model = "m3e", transect_angle = 0, start_corner = "top_left",
      reverse_transect_order = FALSE, trim_transect_length = FALSE,
      show_transects = TRUE, show_photopoints = TRUE, show_footprints = FALSE,
      pilot_distance_m = 900, limit_pilot_radius = FALSE,
      # This test is about the radius; its pilot sits on land at the Strand.
      exclude_land = FALSE, avoid_no_fly_zones = FALSE)
    expect_equal(export_filename("AOI", ".kml", "default.kml", "20261104"), "default.kml")
    session$setInputs(export_name = "Bowen Site 01")
    for (kind in c("AOI", "DJI_Mission", "Litchi_Bundle", "Plan_Summary", "Litchi", "Litchi_New_Hub", "Import_Notes")) {
      expect_equal(export_filename(kind, ".test", "default.test", "20261104"),
        paste0("Bowen_Site_01_M3E_", kind, "_20261104.test"))
    }
    session$setInputs(export_name = "")
    pilot <- c(146.8169, -19.259)
    polygon <- local_line(pilot, rbind(c(-1100, -1100), c(1100, -1100),
      c(1100, 1100), c(-1100, 1100), c(-1100, -1100)))
    drawn_polygon(data.frame(lng = polygon[, 1], lat = polygon[, 2]))
    session$flushReact()
    unrestricted <- build_plan_summary_data()
    expect_equal(unrestricted$metrics$forward_overlap_pct, 70)
    expect_equal(unrestricted$metrics$sidelap_pct, 0)
    session$setInputs(transect_gap_m = camera_params()$footprint_width_ratio * mission_altitude() / 10)
    expect_equal(build_plan_summary_data()$metrics$sidelap_pct, -10)
    session$setInputs(transect_gap_m = 0)
    session$setInputs(limit_pilot_radius = TRUE)
    expect_error(build_plan_summary_data(), "Place the pilot")
    session$setInputs(place_pilot = 1)
    session$setInputs(map_click = list(lng = pilot[1], lat = pilot[2]))
    expect_equal(pilot_point(), pilot)
    expect_false(placing_pilot())
    plan <- build_plan_summary_data()
    expect_true(plan$metrics$pilot_radius_enabled)
    expect_lt(plan$metrics$route_distance_km, unrestricted$metrics$route_distance_km)
    result <- generate_transects_and_photopoints(drawn_polygon(), mission_altitude(),
      input$transect_gap_m, camera_params(), 0, FALSE)
    expect_within_radius(result$transects, pilot, 900)
    expect_equal(generate_transects_and_photopoints(drawn_polygon(), mission_altitude(),
      input$transect_gap_m, camera_params(), 0, TRUE)$transects, result$transects)
    photos <- do.call(rbind, result$photo_points)
    expect_lte(max(geosphere::distGeo(pilot, photos[, 1:2])), 900)
    native <- litchi_native_plan()
    expect_lte(max(geosphere::distGeo(pilot,
      as.matrix(native[, c("longitude", "latitude")]))), 900)
    records <- app_env$build_dji_waypoint_records(result$transects)
    points <- do.call(rbind, lapply(records$waypoints, function(p) c(p$lng, p$lat)))
    expect_lte(max(geosphere::distGeo(pilot, points)), 900)
    for (xml in list(
      app_env$generate_template_kml(result$transects, mission_altitude(), mission_speed(),
        capture_settings()$interval, selected_drone()),
      app_env$generate_waylines_wpml(result$transects, mission_altitude(), mission_speed(),
        capture_settings()$interval, selected_drone())
    )) {
      exported_points <- app_env$extract_kml_coordinate_pairs(xml)
      expect_lte(max(geosphere::distGeo(pilot, exported_points)), 900)
    }
    report <- tempfile(fileext = ".html")
    app_env$write_plan_summary_html(report, plan)
    html <- paste(readLines(report, warn = FALSE), collapse = "\n")
    expect_match(html, '"pilot_radius_m":900', fixed = TRUE)
    expect_match(html, '"airspace_status":"checked"', fixed = TRUE)
    expect_match(html, "Airspace check", fixed = TRUE)
    expect_match(paste(unlist(output$mission_summary), collapse = ""), "Airspace", fixed = TRUE)
    expect_match(html, '"pilot_boundary":', fixed = TRUE)
    expect_match(html, '"forward_overlap_pct":70', fixed = TRUE)
    expect_match(html, '"sidelap_pct":0', fixed = TRUE)
    expect_match(html, 'Forward overlap', fixed = TRUE)
    expect_false(grepl('achieved', html, ignore.case = TRUE))
    expect_match(html, 'Sidelap', fixed = TRUE)
    if (nzchar(Sys.which("node"))) {
      script_file <- tempfile(fileext = ".js")
      script <- sub("(?s).*<script>", "", html, perl = TRUE)
      script <- sub("(?s)</script>.*", "", script, perl = TRUE)
      writeLines(script, script_file)
      syntax_check <- system2("node", c("--check", shQuote(script_file)), stdout = TRUE, stderr = TRUE)
      expect_null(attr(syntax_check, "status"))
      unlink(script_file)
    }
    unlink(report)
    session$setInputs(export_name = "Bowen")
    archive <- tempfile()
    bundle_files <- write_complete_plan(archive)
    entries <- zip::zip_list(archive)$filename
    expect_length(entries, 5L)
    expect_true(all(c(unname(bundle_files), "README.txt") %in% entries))
    expect_true(all(startsWith(bundle_files, "Bowen_M3E_")))
    extracted <- tempfile()
    dir.create(extracted)
    utils::unzip(archive, exdir = extracted)
    readme <- paste(readLines(file.path(extracted, "README.txt")), collapse = "\n")
    expect_match(readme, "Transfer the KMZ to the drone RC via SD card", fixed = TRUE)
    expect_match(readme, "Transfer the extracted Litchi plan files to the drone RC via SD card", fixed = TRUE)
    expect_match(readme, "forward overlap, sidelap", fixed = TRUE)
    expect_true(all(c("wpmz/template.kml", "wpmz/waylines.wpml") %in%
      zip::zip_list(file.path(extracted, bundle_files[["kmz"]]))$filename))
    litchi_entries <- zip::zip_list(file.path(extracted, bundle_files[["litchi"]]))$filename
    expect_equal(sort(tools::file_ext(litchi_entries)), c("csv", "lchz", "txt"))
    summary <- paste(readLines(file.path(extracted, bundle_files[["summary"]])), collapse = "\n")
    expect_match(summary, '"forward_overlap_pct":70', fixed = TRUE)
    expect_match(summary, '"pilot_radius_m":900', fixed = TRUE)
    expect_match(paste(readLines(file.path(extracted, bundle_files[["aoi"]])), collapse = "\n"),
      "<Polygon>", fixed = TRUE)
    unlink(archive)
    unlink(extracted, recursive = TRUE)
    session$setInputs(export_name = "")
    session$setInputs(pilot_distance_m = 300)
    smaller <- build_plan_summary_data()
    expect_equal(smaller$metrics$pilot_radius_m, 300)
    expect_lt(smaller$metrics$route_distance_km, plan$metrics$route_distance_km)
    session$setInputs(pilot_distance_m = NA_real_)
    expect_error(build_plan_summary_data(), "at least 1 metre")
    session$setInputs(pilot_distance_m = 300)
    session$setInputs(trim_transect_length = TRUE)
    expect_within_radius(generate_transects_and_photopoints(drawn_polygon(),
      mission_altitude(), 0, camera_params(), 0, TRUE)$transects, pilot, 300)
    session$setInputs(place_pilot = 2)
    session$setInputs(cancel_place_pilot = 1)
    session$setInputs(map_click = list(lng = 147, lat = -19))
    expect_equal(pilot_point(), pilot)
    session$setInputs(place_pilot = 3)
    session$setInputs(map_click = list(lng = 147, lat = -19))
    expect_error(build_plan_summary_data(), "overlaps")
    session$setInputs(limit_pilot_radius = FALSE, trim_transect_length = FALSE)
    expect_equal(build_plan_summary_data()$metrics$route_distance_km,
      unrestricted$metrics$route_distance_km)
  })
})

test_that("remove pilot clears the marker but keeps the radius option on", {
  shiny::testServer(app_env$server, {
    session$setInputs(limit_pilot_radius = TRUE, pilot_distance_m = 900)
    session$setInputs(place_pilot = 1)
    expect_true(placing_pilot())
    session$setInputs(cancel_place_pilot = 1)
    expect_false(placing_pilot())
    session$setInputs(place_pilot = 2)
    session$setInputs(map_click = list(lng = 146.8, lat = -19.25))
    expect_equal(pilot_point(), c(146.8, -19.25))
    session$setInputs(remove_pilot = 1)
    expect_null(pilot_point())
    expect_true(isTRUE(input$limit_pilot_radius))
  })
})

test_that("aircraft use DJI's published JPEG minimum photo intervals", {
  # enterprise.dji.com specs: M4E 0.5 s, M4T 0.7 s, M3E 0.7 s (JPEG). M3T lists
  # 2 s for JPEG but not at 48 MP, the image size the planner uses, so 3 s.
  models <- app_env$dji_drone_models
  expected <- c(m3e = 0.7, m3t = 3, m4e = 0.5, m4t = 0.7)
  for (id in names(expected)) {
    expect_equal(models[[id]]$min_photo_interval, expected[[id]], info = id)
    expect_equal(min(models[[id]]$photo_intervals), expected[[id]], info = id)
  }
  expect_equal(models$m4e$photo_intervals, c(0.5, 0.7, 1, 2, 3, 5, 7, 10, 15, 20, 30, 60))
  expect_equal(models$m4t$photo_intervals, c(0.7, 1, 2, 3, 5, 7, 10, 15, 20, 30, 60))
  # A fast, low-overlap M4E plan may now use intervals below 1 s.
  capture <- app_env$optimize_capture_settings(models$m4e, altitude_m = 30, front_overlap = 80)
  expect_gte(capture$interval, 0.5)
  expect_lte(capture$speed, models$m4e$max_speed)
  expect_true(app_env$validate_model_capture_settings(models$m4e, capture$speed, capture$interval))
  expect_error(app_env$validate_model_capture_settings(models$m4t, 5, 0.5), "below the 0.70 s minimum")
})

test_that("a drawn boundary with a repeated vertex is accepted", {
  clean <- app_env$clean_aoi_vertices(data.frame(
    lng = c(146.90, 146.92, 146.92, 146.92, 146.90, 146.90),
    lat = c(-19.20, -19.20, -19.20, -19.22, -19.22, -19.20)))
  expect_equal(nrow(clean), 4L)
  expect_silent(app_env$validate_aoi_polygon(clean))
  shiny::testServer(app_env$server, {
    # Double-clicking to finish repeats the last vertex in the drawn GeoJSON.
    ring <- list(list(146.90, -19.20), list(146.92, -19.20), list(146.92, -19.22),
                 list(146.92, -19.22), list(146.90, -19.22), list(146.90, -19.20))
    session$setInputs(map_draw_new_feature = list(type = "Feature",
      geometry = list(type = "Polygon", coordinates = list(ring))))
    expect_equal(nrow(drawn_polygon()), 4L)
  })
})

test_that("M4E plans export at intervals below 0.7 s", {
  shiny::testServer(app_env$server, {
    session$setInputs(altitude = 40, front_overlap = 80, transect_gap_m = 0,
      drone_model = "m4e", transect_angle = 0, start_corner = "top_right",
      reverse_transect_order = FALSE, trim_transect_length = FALSE,
      show_transects = FALSE, show_photopoints = FALSE, show_footprints = FALSE,
      pilot_distance_m = 900, limit_pilot_radius = FALSE, export_name = "")
    drawn_polygon(data.frame(lng = c(147.20, 147.21, 147.21, 147.20),
                             lat = c(-19.005, -19.005, -19.00, -19.00)))
    session$flushReact()
    expect_lt(capture_settings()$interval, 0.7)
    archive <- tempfile(fileext = ".zip")
    expect_no_error(write_complete_plan(archive))
    expect_true(any(grepl("Litchi_Bundle.*[.]zip$", zip::zip_list(archive)$filename)))
  })
})
