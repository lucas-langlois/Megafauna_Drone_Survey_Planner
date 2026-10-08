skip_if_not_installed("terra", minimum_version = "1.6.3")

helper_path <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "local_basemap.R"),
  mustWork = TRUE
)
repo_root <- dirname(helper_path)
source(helper_path, local = FALSE)

write_test_raster <- function(raster) {
  path <- tempfile(fileext = ".tif")
  terra::writeRaster(raster, path, overwrite = TRUE)
  path
}

test_that("a geographic GeoTIFF returns ordered WGS84 bounds", {
  raster <- terra::rast(
    nrows = 4,
    ncols = 5,
    xmin = 145,
    xmax = 146,
    ymin = -20,
    ymax = -19,
    crs = "EPSG:4326"
  )
  terra::values(raster) <- seq_len(terra::ncell(raster))
  path <- write_test_raster(raster)
  on.exit(unlink(path), add = TRUE)

  result <- load_local_basemap(path)

  expect_s4_class(result$raster, "SpatRaster")
  expect_equal(unname(result$bounds), c(145, -20, 146, -19), tolerance = 1e-7)
})

test_that("a projected GeoTIFF has correctly transformed WGS84 bounds", {
  raster <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 16100000,
    xmax = 16200000,
    ymin = -2300000,
    ymax = -2200000,
    crs = "EPSG:3857"
  )
  terra::values(raster) <- seq_len(terra::ncell(raster))
  path <- write_test_raster(raster)
  on.exit(unlink(path), add = TRUE)

  result <- load_local_basemap(path)

  expect_true(all(is.finite(result$bounds)))
  expect_lt(result$bounds[["xmin"]], result$bounds[["xmax"]])
  expect_lt(result$bounds[["ymin"]], result$bounds[["ymax"]])
  expect_true(result$bounds[["xmin"]] >= -180 && result$bounds[["xmax"]] <= 180)
  expect_true(result$bounds[["ymin"]] >= -90 && result$bounds[["ymax"]] <= 90)
  expect_match(terra::crs(result$raster), "3857|Mercator", ignore.case = TRUE)
})

test_that("a raster without a CRS is rejected", {
  raster <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(raster) <- 1:4
  terra::crs(raster) <- ""

  expect_error(validate_local_basemap_raster(raster), "no CRS")
})

test_that("a missing file is rejected", {
  expect_error(load_local_basemap(tempfile(fileext = ".tif")), "File not found")
})

test_that("invalid WGS84 bounds are rejected", {
  expect_error(validate_wgs84_bounds(c(0, 0, Inf, 1)), "finite")
  expect_error(validate_wgs84_bounds(c(181, 0, 182, 1)), "outside")
  expect_error(validate_wgs84_bounds(c(1, 1, 1, 2)), "ordered")
})

test_that("adjacent GeoTIFF tiles form one full-resolution virtual mosaic", {
  first <- terra::rast(
    nrows = 2, ncols = 2,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  second <- terra::rast(
    nrows = 2, ncols = 2,
    xmin = 1, xmax = 2, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(first) <- 1:4
  terra::values(second) <- 5:8
  first_path <- write_test_raster(first)
  second_path <- write_test_raster(second)
  on.exit(unlink(c(first_path, second_path)), add = TRUE)

  result <- load_local_basemap_files(c(first_path, second_path))

  expect_equal(result$source_count, 2L)
  expect_identical(result$source_files, c(first_path, second_path))
  expect_equal(c(terra::nrow(result$raster), terra::ncol(result$raster)), c(2, 4))
  expect_equal(unname(result$bounds), c(0, 0, 2, 1), tolerance = 1e-7)
  expect_true(file.exists(result$source_file))

  widget <- add_local_geotiff_dependencies(leaflet::leaflet())
  expect_silent(add_local_geotiff_url(
    widget,
    url = "session/test/dataobj/tile",
    layer_id = "local-geotiff-1",
    group = "Local GeoTIFF",
    attempt_id = 1L
  ))
})

test_that("all global quarterly mosaic tiles validate for streamed Leaflet display", {
  fixture_dir <- file.path(repo_root, "global_quarterly_2026q2_mosaic")
  skip_if_not(dir.exists(fixture_dir), "Real GeoTIFF fixture directory is not available")

  tiles <- sort(list.files(
    fixture_dir,
    pattern = "\\.(tif|tiff)$",
    full.names = TRUE,
    ignore.case = TRUE
  ))
  expect_gte(length(tiles), 10L)

  results <- lapply(tiles, load_local_basemap)
  for (result in results) {
    expect_true(all(is.finite(result$bounds)))
    expect_lt(result$bounds[["xmin"]], result$bounds[["xmax"]])
    expect_lt(result$bounds[["ymin"]], result$bounds[["ymax"]])
    expect_true(result$bounds[["xmin"]] >= -180 && result$bounds[["xmax"]] <= 180)
    expect_true(result$bounds[["ymin"]] >= -90 && result$bounds[["ymax"]] <= 90)
  }

  mosaic <- load_local_basemap_files(tiles)
  expect_equal(mosaic$source_count, length(tiles))
  expect_gte(terra::nrow(mosaic$raster), max(vapply(results, function(x) terra::nrow(x$raster), numeric(1))))
  expect_gte(terra::ncol(mosaic$raster), max(vapply(results, function(x) terra::ncol(x$raster), numeric(1))))
  expect_true(all(is.finite(mosaic$bounds)))

  expect_identical(mosaic$source_files, tiles)
  widget <- add_local_geotiff_dependencies(leaflet::leaflet())
  expect_silent(add_local_geotiff_url(
    widget,
    url = "session/test/dataobj/fixture",
    layer_id = "local-geotiff-1",
    group = "Local GeoTIFF",
    attempt_id = 1L
  ))
})

test_that("GeoTIFF range responder returns only the requested bytes", {
  path <- tempfile(fileext = ".tif")
  writeBin(as.raw(0:255), path)

  response <- local_geotiff_range_response(
    path,
    list(REQUEST_METHOD = "GET", HTTP_RANGE = "bytes=10-19")
  )

  expect_identical(response$status, 206L)
  expect_identical(response$headers[["Accept-Ranges"]], "bytes")
  expect_identical(response$headers[["Content-Range"]], "bytes 10-19/256")
  expect_identical(response$headers[["Content-Length"]], "10")
  expect_identical(response$body, as.raw(10:19))
})

test_that("GeoTIFF range responder handles HEAD and invalid ranges", {
  path <- tempfile(fileext = ".tif")
  writeBin(as.raw(0:99), path)

  head <- local_geotiff_range_response(path, list(REQUEST_METHOD = "HEAD"))
  expect_identical(head$status, 200L)
  expect_identical(head$headers[["Content-Length"]], "100")
  expect_length(head$body, 0L)

  invalid <- local_geotiff_range_response(
    path,
    list(REQUEST_METHOD = "GET", HTTP_RANGE = "bytes=200-300")
  )
  expect_identical(invalid$status, 416L)
  expect_identical(invalid$headers[["Content-Range"]], "bytes */100")
})
