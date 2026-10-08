# Session-local advisory restriction boundaries. No live airspace feed.
# Each app directory keeps an identical copy so both editions run standalone.
restriction_group <- "No-fly / restriction zones (imported)"
restriction_max_features <- 2000L

read_restriction_zones <- function(path, filename, max_features = restriction_max_features) {
  extension <- tolower(tools::file_ext(filename))
  if (!extension %in% c("geojson", "json", "kml")) {
    stop("Upload polygon boundaries as GeoJSON or KML.")
  }
  local_path <- tempfile(fileext = paste0(".", extension))
  on.exit(unlink(local_path), add = TRUE)
  if (!file.copy(path, local_path)) stop("Could not read the uploaded boundaries.")

  layer_names <- tryCatch(
    {
      layers <- sf::st_layers(local_path)
      if (is.null(layers) || nrow(layers) == 0) character(0) else layers$name
    },
    error = function(error) character(0)
  )
  # GeoJSON exposes a single layer; KML folders can expose several.
  if (length(layer_names) == 0) layer_names <- NA_character_

  geometries <- list()
  labels <- character()
  ignored_non_polygon <- 0L
  failed_layers <- character()
  source_crs <- NULL

  for (layer_name in layer_names) {
    features <- tryCatch(
      if (is.na(layer_name)) {
        sf::st_read(local_path, quiet = TRUE)
      } else {
        sf::st_read(local_path, layer = layer_name, quiet = TRUE)
      },
      error = function(error) NULL
    )
    if (is.null(features) || nrow(features) == 0) {
      # A layer that cannot be read is a coverage gap, not silent success.
      if (is.null(features) && !is.na(layer_name)) failed_layers <- c(failed_layers, layer_name)
      next
    }
    if (is.null(source_crs) && !is.na(sf::st_crs(features))) source_crs <- sf::st_crs(features)
    types <- as.character(sf::st_geometry_type(features))

    # Pull polygon parts out of geometry collections (for example OK2Fly
    # exports mixing tracks and areas) instead of dropping the whole file.
    collection_rows <- which(types == "GEOMETRYCOLLECTION")
    if (length(collection_rows) > 0) {
      extracted <- tryCatch(
        suppressWarnings(sf::st_collection_extract(features[collection_rows, ], "POLYGON")),
        error = function(error) NULL
      )
      if (!is.null(extracted)) {
        extracted_types <- as.character(sf::st_geometry_type(extracted))
        keep <- extracted_types %in% c("POLYGON", "MULTIPOLYGON")
        if (any(keep)) {
          geometries <- c(geometries, list(sf::st_geometry(extracted[keep, ])))
          extracted_labels <- restriction_zone_labels(extracted[keep, , drop = FALSE])
          labels <- c(labels, extracted_labels)
        }
        ignored_non_polygon <- ignored_non_polygon + sum(!keep)
      } else {
        ignored_non_polygon <- ignored_non_polygon + length(collection_rows)
      }
      features <- features[-collection_rows, , drop = FALSE]
      if (nrow(features) == 0) next
      types <- as.character(sf::st_geometry_type(features))
    }

    polygon_rows <- types %in% c("POLYGON", "MULTIPOLYGON")
    ignored_non_polygon <- ignored_non_polygon + sum(!polygon_rows)
    if (!any(polygon_rows)) next
    polygon_features <- features[polygon_rows, , drop = FALSE]
    geometries <- c(geometries, list(sf::st_geometry(polygon_features)))
    labels <- c(labels, restriction_zone_labels(polygon_features))
  }

  if (length(failed_layers) > 0) {
    stop(sprintf(
      "Could not read %d KML layer(s) (%s). Re-export the file before importing.",
      length(failed_layers), paste(utils::head(failed_layers, 3), collapse = ", ")
    ))
  }
  if (length(geometries) == 0 || length(labels) == 0) {
    if (ignored_non_polygon > 0) {
      stop(sprintf(
        "No polygons found. Ignored %d non-polygon feature(s); upload polygon boundaries.",
        ignored_non_polygon
      ))
    }
    stop("The file contains no restriction boundaries.")
  }

  geometry <- do.call(c, geometries)
  zones <- sf::st_sf(zone_label_raw = labels, geometry = geometry)
  if (nrow(zones) > max_features) {
    stop(sprintf("Too many boundaries (%d). This import is limited to %d.", nrow(zones), max_features))
  }

  if (is.na(sf::st_crs(zones))) {
    # Genuine KML and RFC 7946 GeoJSON are WGS84. Accept a missing CRS only
    # when every coordinate already sits inside valid lon/lat ranges.
    coords <- tryCatch(sf::st_coordinates(zones), error = function(error) NULL)
    if (is.null(coords) || !all(is.finite(coords[, c("X", "Y")])) ||
        any(abs(coords[, "X"]) > 180) || any(abs(coords[, "Y"]) > 90)) {
      stop("Restriction boundaries must include a coordinate reference system.")
    }
    sf::st_crs(zones) <- 4326
  }
  zones <- sf::st_zm(sf::st_transform(zones, 4326), drop = TRUE, what = "ZM")
  if (any(sf::st_is_empty(zones))) stop("Restriction boundaries contain empty polygons.")
  valid <- sf::st_is_valid(zones)
  if (any(is.na(valid) | !valid)) {
    stop("Restriction boundaries contain invalid polygons. Repair the file and re-import.")
  }
  coordinates <- sf::st_coordinates(zones)
  if (any(!is.finite(coordinates[, c("X", "Y")])) ||
      any(abs(coordinates[, "X"]) > 180) || any(abs(coordinates[, "Y"]) > 90)) {
    stop("Restriction boundaries contain invalid geographic coordinates.")
  }
  missing <- is.na(zones$zone_label_raw) | !nzchar(trimws(zones$zone_label_raw))
  zones$zone_label_raw[missing] <- paste("Restriction zone", which(missing))
  zones <- sf::st_sf(zone_name = as.character(zones$zone_label_raw), geometry = sf::st_geometry(zones))
  attr(zones, "ignored_non_polygon") <- ignored_non_polygon
  zones
}

restriction_zone_labels <- function(features) {
  name_column <- intersect(c("name", "Name", "NAME"), names(features))
  if (length(name_column)) as.character(features[[name_column[[1]]]]) else rep(NA_character_, nrow(features))
}

# Count imported zones intersecting the AOI, including boundary contact.
# Returns NA when the check itself fails so callers show "unavailable".
restriction_overlap_count <- function(polygon, zones) {
  if (is.null(polygon) || is.null(zones)) return(0L)
  tryCatch(
    {
      coordinates <- as.matrix(polygon[, c("lng", "lat")])
      if (!all(coordinates[1, ] == coordinates[nrow(coordinates), ])) {
        coordinates <- rbind(coordinates, coordinates[1, ])
      }
      area <- sf::st_sfc(sf::st_polygon(list(coordinates)), crs = 4326)
      length(sf::st_intersects(area, zones)[[1]])
    },
    error = function(error) NA_integer_
  )
}

restriction_tools_ui <- function() {
  shiny::tags$details(
    class = "aoi-resize-panel",
    shiny::tags$summary("No-fly / restriction zones (advisory import)"),
    shiny::fileInput("restriction_file", "Import restriction boundaries (GeoJSON/KML)",
                     accept = c(".geojson", ".json", ".kml")),
    shiny::actionButton("clear_restrictions", "Remove restriction boundaries", class = "btn-default btn-sm"),
    shiny::uiOutput("restriction_status"),
    shiny::tags$p(class = "help-block",
      "Imported boundaries are advisory only. They do not include live NOTAMs, controlled-airspace approvals, altitude limits, or permit conditions. No live nationwide restriction feed is bundled with this planner. Check current permissions before flying."),
    shiny::tags$p(class = "help-block",
      "Workflow: use Export AOI as KML below, then load that KML in OK2Fly (Load Geometry File) and run its flight check. OK2Fly Web geometry tools need a subscription."),
    shiny::tags$a("OK2Fly", href = "https://ok2fly.com.au/", target = "_blank", rel = "noopener"),
    " | ",
    shiny::tags$a("CASA drone safety apps", href = "https://www.casa.gov.au/knowyourdrone/drone-safety-apps", target = "_blank", rel = "noopener"),
    shiny::tags$br(),
    shiny::tags$a("Drone rule digitisation (local rules)", href = "https://www.drones.gov.au/policies-and-programs/initiatives/drone-rule-digitisation", target = "_blank", rel = "noopener"),
    " | ",
    shiny::tags$a("Airservices data products (subscription)", href = "https://data.airservicesaustralia.com/", target = "_blank", rel = "noopener")
  )
}

restriction_tools_server <- function(input, output, session, drawn_polygon) {
  zones <- shiny::reactiveVal(NULL)
  source_name <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$restriction_file, {
    uploaded <- tryCatch(
      read_restriction_zones(input$restriction_file$datapath, input$restriction_file$name),
      error = function(error) {
        shiny::showNotification(paste("Could not import restrictions:", conditionMessage(error)), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(uploaded)) return()
    ignored <- attr(uploaded, "ignored_non_polygon")
    if (!is.null(ignored) && is.finite(ignored) && ignored > 0) {
      shiny::showNotification(sprintf("Ignored %d non-polygon feature(s).", ignored), type = "warning", duration = 6)
    }
    leaflet::leafletProxy("map", session) %>%
      leaflet::clearGroup(restriction_group) %>%
      leaflet::addPolygons(data = uploaded, group = restriction_group,
        color = "#c62828", weight = 2, fillOpacity = 0.2,
        label = as.character(htmltools::htmlEscape(uploaded$zone_name)),
        popup = as.character(htmltools::htmlEscape(uploaded$zone_name))) %>%
      leaflet::showGroup(restriction_group)
    zones(uploaded)
    source_name(input$restriction_file$name)
  })
  shiny::observeEvent(input$clear_restrictions, {
    leaflet::leafletProxy("map", session) %>% leaflet::clearGroup(restriction_group)
    zones(NULL)
    source_name(NULL)
    shinyjs::reset("restriction_file")
  })
  output$restriction_status <- shiny::renderUI({
    if (is.null(zones())) return(shiny::tags$p("No restriction data loaded. Coverage is unknown."))
    count <- restriction_overlap_count(drawn_polygon(), zones())
    ignored <- attr(zones(), "ignored_non_polygon")
    shiny::tagList(
      shiny::tags$p(sprintf("Loaded %d boundaries from %s. Toggle them in the map layer control; hiding the overlay does not turn off warnings.",
        nrow(zones()), source_name())),
      if (!is.null(ignored) && is.finite(ignored) && ignored > 0)
        shiny::tags$p(sprintf("Ignored %d non-polygon feature(s) in that file.", ignored)),
      if (is.null(drawn_polygon())) shiny::tags$p("Draw or import a survey area to check overlap.")
      else if (is.na(count)) shiny::tags$p(role = "alert", style = "color:#a51d1d;font-weight:bold;",
        "Overlap check is unavailable for this survey area. Verify restrictions manually.")
      else if (count > 0L) shiny::tags$p(role = "alert", style = "color:#a51d1d;font-weight:bold;",
        sprintf("Survey area intersects %d imported restriction zone(s), including any boundary contact. Check permissions and current conditions. Planning and export stay available.", count))
      else shiny::tags$p("No overlap with imported boundaries. This does not establish permission to fly.")
    )
  })
  zones
}
