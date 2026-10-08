# ==============================================================================
# Local GeoTIFF basemap helper
#
# Loads a local GeoTIFF uploaded through the app's file input, validates it,
# and returns the raster together with ordered WGS84 bounds. The raster is
# kept only for the current Shiny session; no permanent copy is written.
# ==============================================================================

# Validate that bounds are finite, ordered (west < east, south < north) and
# inside valid WGS84 longitude/latitude ranges.
validate_wgs84_bounds <- function(b) {
  b <- as.numeric(b)
  if (length(b) != 4L || !all(is.finite(b))) {
    stop("Bounds must be four finite values (xmin, ymin, xmax, ymax).")
  }
  xmin <- min(b[1], b[3]); xmax <- max(b[1], b[3])
  ymin <- min(b[2], b[4]); ymax <- max(b[2], b[4])
  if (xmin >= xmax || ymin >= ymax) {
    stop("Bounds are not ordered: require xmin < xmax and ymin < ymax.")
  }
  if (xmin < -180 || xmax > 180 || ymin < -90 || ymax > 90) {
    stop("Bounds fall outside valid WGS84 longitude/latitude ranges.")
  }
  c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
}

# Derive browser display ranges without reading an entire large raster. Eight-
# bit imagery uses its native 0..255 range; other data types are sampled on a
# regular grid so streamed 16-bit and floating-point imagery retains contrast.
local_basemap_display_ranges <- function(r, sample_size = 10000L) {
  band_count <- terra::nlyr(r)
  data_types <- terra::datatype(r)
  if (length(data_types) == band_count && all(data_types == "INT1U")) {
    return(list(mins = rep(0, band_count), maxs = rep(255, band_count)))
  }

  sample_size <- min(as.integer(sample_size), terra::ncell(r))
  sampled <- terra::spatSample(
    r,
    size = sample_size,
    method = "regular",
    na.rm = TRUE,
    values = TRUE,
    as.df = TRUE
  )
  ranges <- lapply(sampled, function(values) {
    values <- as.numeric(values)
    values <- values[is.finite(values)]
    if (!length(values)) c(0, 255) else range(values)
  })
  list(
    mins = vapply(ranges, `[[`, numeric(1), 1L),
    maxs = vapply(ranges, `[[`, numeric(1), 2L)
  )
}

# Validate an already-open SpatRaster and calculate its WGS84 bounds. Keeping
# this separate makes CRS and extent validation testable without relying on
# GDAL's format-specific defaults when writing a synthetic file.
validate_local_basemap_raster <- function(r) {
  if (is.null(r) || !inherits(r, "SpatRaster")) {
    stop("File did not load as a raster.")
  }
  if (terra::nlyr(r) < 1L) {
    stop("Raster has no layers.")
  }
  if (terra::ncell(r) < 1L || terra::nrow(r) < 1L || terra::ncol(r) < 1L) {
    stop("Raster has invalid dimensions.")
  }

  crs_raw <- terra::crs(r)
  if (is.null(crs_raw) || !nzchar(crs_raw) ||
      grepl("^NA$|^$", crs_raw, ignore.case = TRUE)) {
    stop("Raster has no CRS; a defined coordinate reference system is required.")
  }

  # Many imagery GeoTIFFs contain RGB or RGBA bands but omit GDAL colour
  # interpretation metadata. Mark conventional three- and four-band rasters
  # explicitly so Leaflet renders colour instead of silently using band one.
  if (terra::nlyr(r) %in% c(3L, 4L) && !terra::has.RGB(r)) {
    terra::RGB(r) <- seq_len(terra::nlyr(r))
  }

  source_extent <- as.vector(terra::ext(r))
  if (length(source_extent) != 4L || !all(is.finite(source_extent))) {
    stop("Raster extent must contain four finite values.")
  }

  # Transform the four extent corners, rather than reprojecting the full
  # raster merely to calculate bounds. Leaflet performs the display
  # reprojection later, and keeping the original raster avoids an expensive
  # duplicate in memory for large uploads.
  corners <- data.frame(
    x = c(source_extent[1], source_extent[1], source_extent[2], source_extent[2]),
    y = c(source_extent[3], source_extent[4], source_extent[3], source_extent[4])
  )
  corner_points <- terra::vect(
    corners,
    geom = c("x", "y"),
    crs = crs_raw
  )
  wgs84_points <- tryCatch(
    terra::project(corner_points, "EPSG:4326"),
    error = function(e) {
      stop("Could not transform raster bounds to WGS84: ", conditionMessage(e))
    }
  )
  wgs84_coords <- terra::crds(wgs84_points)
  bounds <- validate_wgs84_bounds(c(
    min(wgs84_coords[, 1]),
    min(wgs84_coords[, 2]),
    max(wgs84_coords[, 1]),
    max(wgs84_coords[, 2])
  ))

  list(
    raster = r,
    bounds = bounds,
    crs = crs_raw
  )
}

# Load and validate a local GeoTIFF for use as an offline Leaflet basemap.
#
# Returns list(raster = SpatRaster, bounds = c(xmin, ymin, xmax, ymax) in
# WGS84 degrees, crs = source CRS string). Errors have user-readable messages.
load_local_basemap <- function(path) {
  if (missing(path) || is.null(path) || length(path) != 1L || !nzchar(path)) {
    stop("No GeoTIFF file path supplied.")
  }
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  file_size <- file.info(path)$size
  if (!is.finite(file_size)) {
    stop("GeoTIFF file size cannot be read.")
  }

  raster <- tryCatch(
    terra::rast(path),
    error = function(e) stop("Could not open GeoTIFF: ", conditionMessage(e))
  )
  validate_local_basemap_raster(raster)
}

# Load one or more adjacent, compatible GeoTIFF tiles as a virtual mosaic.
# The VRT lives in the R session's temporary directory and references Shiny's
# temporary upload files, so no permanent mosaic copy is created.
load_local_basemap_files <- function(paths) {
  if (is.null(paths) || !length(paths) || any(!nzchar(paths))) {
    stop("No GeoTIFF file paths supplied.")
  }
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths)) {
    stop("File not found: ", missing_paths[[1]])
  }

  file_sizes <- file.info(paths)$size
  total_size <- sum(file_sizes)
  if (any(!is.finite(file_sizes))) {
    stop("A GeoTIFF file size cannot be read.")
  }

  if (length(paths) == 1L) {
    result <- load_local_basemap(paths[[1]])
    result$source_count <- 1L
    result$total_size <- total_size
    result$source_file <- paths[[1]]
    result$source_files <- paths
    result$display_ranges <- list(local_basemap_display_ranges(result$raster))
    return(result)
  }

  rasters <- lapply(paths, function(path) {
    raster <- tryCatch(
      terra::rast(path),
      error = function(e) stop("Could not open GeoTIFF ", basename(path), ": ", conditionMessage(e))
    )
    validate_local_basemap_raster(raster)$raster
  })
  reference <- rasters[[1]]
  reference_res <- terra::res(reference)
  reference_origin <- terra::origin(reference)
  reference_layers <- terra::nlyr(reference)
  display_ranges <- lapply(rasters, local_basemap_display_ranges)

  for (index in seq_along(rasters)[-1]) {
    raster <- rasters[[index]]
    if (!terra::same.crs(reference, raster)) {
      stop("All GeoTIFF tiles must use the same CRS.")
    }
    if (terra::nlyr(raster) != reference_layers) {
      stop("All GeoTIFF tiles must have the same number of bands.")
    }
    resolution_delta <- max(abs(terra::res(raster) - reference_res))
    origin_delta <- max(abs(terra::origin(raster) - reference_origin))
    alignment_tolerance <- max(abs(reference_res)) * 1e-6
    if (resolution_delta > alignment_tolerance || origin_delta > alignment_tolerance) {
      stop("All GeoTIFF tiles must use the same resolution and pixel alignment.")
    }
  }

  vrt_path <- tempfile("local_basemap_", fileext = ".vrt")
  mosaic <- tryCatch(
    terra::vrt(paths, filename = vrt_path, overwrite = TRUE),
    error = function(e) stop("Could not assemble GeoTIFF mosaic: ", conditionMessage(e))
  )
  result <- validate_local_basemap_raster(mosaic)
  result$source_count <- length(paths)
  result$total_size <- total_size
  result$virtual_mosaic_path <- vrt_path
  result$source_file <- vrt_path
  result$source_files <- paths
  result$display_ranges <- display_ranges
  result
}

# Add the browser libraries used by the streaming GeoTIFF layer. The custom
# binding passes a URL directly to parseGeoraster so tiled GeoTIFF/COG blocks
# are fetched on demand instead of reading the complete raster into memory.
add_local_geotiff_dependencies <- function(map) {
  georaster_dependencies <- getFromNamespace(
    "leafletGeoRasterDependencies",
    "leafem"
  )()
  local_dependency <- htmltools::htmlDependency(
    name = "local-geotiff-stream",
    version = "1.0.5",
    src = c(file = file.path(getwd(), "www")),
    script = "local-geotiff.js"
  )
  map$dependencies <- c(
    map$dependencies,
    georaster_dependencies,
    list(local_dependency)
  )
  map
}

# Add one session-served GeoTIFF URL to the shared local-basemap group.
add_local_geotiff_url <- function(
    map,
    url,
    layer_id,
    group,
    attempt_id,
    mins = NULL,
    maxs = NULL,
    resolution = 256L) {
  leaflet::invokeMethod(
    map,
    data = leaflet::getMapData(map),
    method = "addLocalGeotiffUrl",
    url,
    group,
    layer_id,
    as.integer(resolution),
    as.integer(attempt_id),
    as.list(as.numeric(mins)),
    as.list(as.numeric(maxs))
  )
}

# Build an HTTP response for one GeoTIFF byte-range request. Browser-side COG
# readers rely on 206 responses; Shiny's normal static resource handler returns
# the whole file even when a Range header is supplied.
local_geotiff_range_response <- function(path, request) {
  file_size <- unname(file.info(path)$size)
  if (!is.finite(file_size) || file_size < 1) {
    return(list(status = 404L, headers = list(), body = "GeoTIFF not found"))
  }

  common_headers <- list(
    "Content-Type" = "image/tiff",
    "Accept-Ranges" = "bytes",
    "Cache-Control" = "private, max-age=3600"
  )
  method <- toupper(if (!is.null(request$REQUEST_METHOD)) request$REQUEST_METHOD else "GET")
  range_header <- request$HTTP_RANGE

  if (identical(method, "HEAD")) {
    common_headers[["Content-Length"]] <- as.character(file_size)
    return(list(status = 200L, headers = common_headers, body = raw()))
  }

  # A URL-backed parseGeoraster requests byte ranges. Reject a non-range GET
  # instead of accidentally allocating an arbitrarily large full file in R.
  if (is.null(range_header) || !nzchar(range_header)) {
    return(list(
      status = 400L,
      headers = c(common_headers, list("Content-Type" = "text/plain")),
      body = "This GeoTIFF endpoint requires an HTTP Range header."
    ))
  }

  match <- regexec("^bytes=([0-9]*)-([0-9]*)$", range_header)
  parts <- regmatches(range_header, match)[[1]]
  if (length(parts) != 3L || (!nzchar(parts[[2]]) && !nzchar(parts[[3]]))) {
    return(list(
      status = 416L,
      headers = c(common_headers, list("Content-Range" = paste0("bytes */", file_size))),
      body = raw()
    ))
  }

  if (nzchar(parts[[2]])) {
    start <- as.numeric(parts[[2]])
    end <- if (nzchar(parts[[3]])) as.numeric(parts[[3]]) else file_size - 1
  } else {
    suffix_length <- as.numeric(parts[[3]])
    start <- max(0, file_size - suffix_length)
    end <- file_size - 1
  }
  end <- min(end, file_size - 1)
  if (!is.finite(start) || !is.finite(end) || start < 0 || start > end || start >= file_size) {
    return(list(
      status = 416L,
      headers = c(common_headers, list("Content-Range" = paste0("bytes */", file_size))),
      body = raw()
    ))
  }

  requested_length <- as.integer(end - start + 1)
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  seek(connection, where = start, origin = "start")
  body <- readBin(connection, what = "raw", n = requested_length)
  common_headers[["Content-Length"]] <- as.character(length(body))
  common_headers[["Content-Range"]] <- sprintf(
    "bytes %.0f-%.0f/%.0f",
    start,
    start + length(body) - 1,
    file_size
  )
  list(status = 206L, headers = common_headers, body = body)
}

# Register a GeoTIFF as a session-owned URL backed by the range responder.
register_local_geotiff <- function(session, path, name) {
  session$registerDataObj(
    name,
    path,
    function(data, request) local_geotiff_range_response(data, request)
  )
}
