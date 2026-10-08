# /app.R

# ==============================================================================
# Megafauna Drone Survey Planner - A Shiny App
#
# This app allows users to plan a megafauna drone survey by interactively
# defining a survey area on a map. It calculates key flight metrics based on
# user-defined parameters.
#
# Required Packages:
# install.packages(c("shiny", "leaflet", "geosphere", "dplyr"))
#
# To Run:
# 1. Save this code as a single file named `app.R`.
# 2. Open R or RStudio.
# 3. Run the command: `shiny::runApp()` in the same directory as the file.
# ==============================================================================

library(shiny)
library(leaflet)
library(geosphere) # For distance and area calculations
library(dplyr)     # For data manipulation
library(sf)        # For spatial data handling

if (!requireNamespace("leaflet.extras", quietly = TRUE)) {
  stop("Please install the 'leaflet.extras' package: install.packages('leaflet.extras')")
}
if (!requireNamespace("zip", quietly = TRUE)) {
  stop("Please install the 'zip' package: install.packages('zip')")
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  stop("Please install the 'shinyjs' package: install.packages('shinyjs')")
}

library(shinyjs)   # For enabling/disabling buttons
library(leaflet.extras) # For draw tools
library(zip)       # For KMZ creation

source("planning_tools.R", local = TRUE)

# Digital Earth Australia OGC Web Map Service. The tidal-composite layer's
# default time is the latest published annual composite.
dea_wms_url <- "https://ows.dea.ga.gov.au/"
dea_tidal_layer <- "ga_s2_tidal_composites_cyear_3"
dea_intertidal_layer <- "ga_s2ls_intertidal_cyear_3"
dea_intertidal_legend_url <- paste0(
  dea_wms_url,
  "legend/ga_s2ls_intertidal_cyear_3/intertidal_extents/legend.png"
)
dea_attribution <- paste0(
  "Digital Earth Australia &copy; Commonwealth of Australia ",
  "(Geoscience Australia), CC BY 4.0"
)
transect_length_limit_m <- 1400

# ==============================================================================
# User Interface (UI)
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  tags$head(tags$style(HTML("
    .mission-metrics-panel {
      padding: 10px 12px;
      margin-bottom: 0;
    }
    .mission-metrics-grid {
      display: grid;
      grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
      gap: 10px 18px;
    }
    .mission-metric-section + .mission-metric-section {
      border-left: 1px solid #ddd;
      padding-left: 18px;
    }
    .mission-metric-heading {
      font-weight: 700;
      margin-bottom: 5px;
      color: #222;
    }
    .mission-metric-row {
      display: flex;
      justify-content: space-between;
      align-items: baseline;
      gap: 8px;
      padding: 1px 0;
      line-height: 1.3;
    }
    .mission-metric-label {
      color: #444;
    }
    .mission-metric-value {
      font-weight: 600;
      text-align: right;
      white-space: nowrap;
    }
    @media (max-width: 1100px) {
      .mission-metrics-grid { grid-template-columns: 1fr; }
      .mission-metric-section + .mission-metric-section {
        border-left: 0;
        border-top: 1px solid #ddd;
        padding-left: 0;
        padding-top: 8px;
      }
    }
    .aoi-resize-panel {
      margin: 10px 0 4px;
      padding: 7px 10px 9px;
      border: 1px solid #d5d5d5;
      border-radius: 4px;
      background: #fafafa;
    }
    .aoi-resize-panel summary {
      cursor: pointer;
      font-weight: 600;
    }
    .aoi-resize-panel .form-group {
      margin: 8px 0 4px;
    }
  "))),
  
  # --- App Title ---
  titlePanel("Megafauna Drone Survey Planner"),

  # --- Main Layout ---
  sidebarLayout(
    # --- Sidebar Panel for Inputs and Outputs ---
    sidebarPanel(
      width = 4,
      h4("Mission Parameters"),
      p("Draw a polygon on the map to define the megafauna survey area. You can also import a KML polygon."),

      # --- KML Import ---
      tags$hr(),
      fileInput(
        "kml_file",
        "Import AOI or Previous Survey Plan (KML/KMZ)",
        accept = c(".kml", ".kmz")
      ),
      downloadButton("download_kml", "Export AOI as KML", class = "btn-info"),
      tags$details(
        class = "aoi-resize-panel",
        tags$summary("Resize or rotate AOI"),
        fluidRow(
          column(
            7,
            numericInput("target_area", "Target area:", value = 1, min = 0.0001, step = 0.1)
          ),
          column(
            5,
            selectInput(
              "target_area_unit",
              "Units:",
              choices = c("Hectares" = "ha", "Acres" = "acres"),
              selected = "ha"
            )
          )
        ),
        actionButton(
          "resize_polygon_area",
          "Apply Area Resize",
          icon = icon("expand-arrows-alt"),
          class = "btn-info btn-sm"
        ),
        tags$hr(style = "margin:10px 0;"),
        sliderInput(
          "polygon_rotation_angle",
          "Rotation angle (degrees):",
          min = -180,
          max = 180,
          value = 0,
          step = 1,
          ticks = TRUE,
          width = "100%"
        ),
        tags$div(
          style = paste(
            "display:flex;justify-content:space-between;",
            "margin:-8px 2px 8px;font-size:11px;color:#666;"
          ),
          tags$span("− Counter-clockwise"),
          tags$span("+ Clockwise")
        ),
        actionButton(
          "rotate_polygon",
          "Rotate AOI",
          icon = icon("sync-alt"),
          class = "btn-info btn-sm"
        ),
        tags$div(
          class = "help-block",
          style = "margin:6px 0 0;font-size:12px;",
          "Rotation uses the AOI centre. Afterwards, use the map Edit tool to move vertices or drag the whole AOI, then Save."
        )
      ),
      tags$details(
        class = "aoi-resize-panel",
        tags$summary("Distance measurement"),
        tags$p(class = "help-block",
          "Use the ruler control at the top-right of the map to measure from shore, the start point, or between any points. Results show metres with kilometres alongside. While drawing the survey polygon, the draw tooltip also shows metric area; the installed draw version does not show per-edge lengths."),
        tags$p(class = "help-block",
          "Measurements are separate from the survey area and mission calculations. Finish or delete a measurement with the control's own options; this never deletes the survey area.")
      ),
      restriction_tools_ui(),
      tags$br(), tags$br(),

      # --- Flight Settings ---
      h5("Flight Settings"),
      radioButtons(
        "planning_basis",
        "Set mission by:",
        choices = c("Flight height" = "height", "Ground Sample Distance (GSD)" = "gsd"),
        selected = "height",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.planning_basis === 'height'",
        numericInput("altitude", "Flight Height Above Home Point (meters):", value = 91.4, min = 10, max = 120, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.planning_basis === 'gsd'",
        numericInput("target_gsd", "Target GSD (cm/pixel):", value = 2.43, min = 0.27, max = 3.20, step = 0.01)
      ),
      uiOutput("height_gsd_conversion_ui"),
      uiOutput("auto_speed_ui"),

      # --- Drone Model & Overlap Settings ---
      tags$hr(),
      h5("Drone & Coverage Settings"),
      selectInput(
        "drone_model",
        "Drone Model:",
        choices = c(
          "DJI Mavic 3 Enterprise (M3E)" = "m3e",
          "DJI Mavic 3 Thermal (M3T)" = "m3t",
          "DJI Matrice 4 Enterprise (M4E)" = "m4e",
          "DJI Matrice 4 Thermal (M4T)" = "m4t"
        ),
        selected = "m3e"
      ),
      uiOutput("camera_model_note"),
      p(tags$b("Survey:"), "Megafauna (80% forward overlap / footprints edge-to-edge)"),
      sliderInput("front_overlap", "Front Overlap (%):", min = 0, max = 95, value = 80, step = 5),
      tags$small(
        "Maximum-efficiency capture is used automatically: the fastest feasible flight speed and calculated photo interval preserve the requested forward overlap."
      ),
      numericInput(
        "transect_gap_m",
        "Gap Between Adjacent Image Footprints (meters):",
        value = 0,
        min = 0,
        step = 1
      ),
      tags$small("0 m places adjacent footprints edge-to-edge (0% sidelap). A larger gap creates negative sidelap."),
      checkboxInput(
        "trim_transect_length",
        "Trim oversized transects to 1.4 km",
        value = FALSE
      ),
      tags$small(
        "Longer lines are shortened around their midpoint. The map shows the resulting survey coverage; confirm actual visual line of sight from the pilot position."
      ),

      # --- Transect Direction ---
      tags$hr(),
      sliderInput("transect_angle", "Transect Direction (degrees from North):", min = 0, max = 180, value = 0, step = 1),
      actionButton("calc_optimal_direction", "Calculate Optimal Direction", icon = icon("compass"), class = "btn-info"),
      tags$br(), tags$br(),
      selectInput(
        "polygon_alignment_axis",
        "Align flight lines with polygon:",
        choices = c(
          "Top/bottom edges (long sides)" = "long",
          "Side edges (short sides)" = "short"
        ),
        selected = "long"
      ),
      actionButton("align_polygon_edges", "Apply Polygon Alignment", icon = icon("arrows-alt"), class = "btn-info"),
      tags$br(), tags$br(),
      selectInput("start_corner", "Mission Start Point:",
        choices = c(
          "Top-Left (NW)" = "top_left",
          "Top-Right (NE)" = "top_right",
          "Bottom-Left (SW)" = "bottom_left",
          "Bottom-Right (SE)" = "bottom_right"
        ),
        selected = "top_right"
      ),
      checkboxInput(
        "reverse_transect_order",
        "Reverse transect order (fly route backwards)",
        value = FALSE
      ),
      tags$small(
        "Moves START to the route's previous endpoint while keeping the same short connector legs."
      ),

      # --- Display Options ---
      tags$hr(),
      checkboxInput("show_transects", "Show Flight Transects (with numbered ends)", value = TRUE),
      checkboxInput("show_photopoints", "Show Photo Points", value = FALSE),
      checkboxInput("show_footprints", "Show Photo Footprints", value = FALSE),

      # --- Action Buttons ---
      tags$hr(),
      actionButton("clear_polygon", "Clear Polygon", icon = icon("trash"), class = "btn-danger"),
      downloadButton("download_kmz", "Export DJI Waypoint KMZ", class = "btn-primary"),
      downloadButton("download_litchi_bundle", "Download Litchi Plan Bundle", class = "btn-success"),
      downloadButton("download_plan_summary", "Download Plan Summary HTML", class = "btn-info"),
      tags$p(
        class = "help-block",
        "ZIP bundle includes CSV, New Litchi Hub (.lchz), and import notes. Review the imported route and interval settings before flying."
      ),

      # --- Calculated Outputs ---
      tags$hr(),
      h4("Calculated Mission Metrics"),
      div(class = "well mission-metrics-panel", htmlOutput("mission_summary"))
    ),

    # --- Main Panel for the Map ---
    mainPanel(
      width = 8,
      leafletOutput("map", height = "85vh") # Make map taller
    )
  )
)

# ==============================================================================
# Helper Functions for KMZ Export
# ==============================================================================

# DJI wide-camera specifications and WPML product enumerations. The M3E wide
# camera supports 0.7-second JPEG capture; WPML timed triggers accept floating-
# point seconds. M3T uses 48 MP, for which DJI does not support the 2-second
# interval.
footprint_ratios_from_diagonal_fov <- function(diagonal_fov_deg,
                                               aspect_width = 4,
                                               aspect_height = 3) {
  diagonal_units <- sqrt(aspect_width^2 + aspect_height^2)
  diagonal_ratio <- 2 * tan(diagonal_fov_deg * pi / 360)
  c(
    width = diagonal_ratio * aspect_width / diagonal_units,
    height = diagonal_ratio * aspect_height / diagonal_units
  )
}

m3t_footprint_ratios <- footprint_ratios_from_diagonal_fov(84)
m4t_footprint_ratios <- footprint_ratios_from_diagonal_fov(82)

dji_drone_models <- list(
  m3e = list(
    label = "DJI Mavic 3 Enterprise (M3E)", file_tag = "M3E",
    drone_enum = 77L, drone_sub_enum = 0L, payload_enum = 66L,
    payload_sub_enum = 0L, image_width = 5280L, image_height = 3956L,
    footprint_width_ratio = 17.3 / 12.3,
    footprint_height_ratio = 13.0 / 12.3,
    max_speed = 15, min_photo_interval = 0.7,
    photo_intervals = c(0.7, 1, 2, 3, 5, 7, 10, 15, 20, 30, 60),
    camera_note = "4/3 wide camera, 20 MP (5280 x 3956)"
  ),
  m3t = list(
    label = "DJI Mavic 3 Thermal (M3T)", file_tag = "M3T",
    drone_enum = 77L, drone_sub_enum = 1L, payload_enum = 67L,
    payload_sub_enum = 0L, image_width = 8000L, image_height = 6000L,
    footprint_width_ratio = unname(m3t_footprint_ratios[["width"]]),
    footprint_height_ratio = unname(m3t_footprint_ratios[["height"]]),
    max_speed = 15, min_photo_interval = 3,
    photo_intervals = c(3, 5, 7, 10, 15, 20, 30, 60),
    camera_note = "1/2-inch wide camera, 48 MP (8000 x 6000; minimum 3 s interval)"
  ),
  m4e = list(
    label = "DJI Matrice 4 Enterprise (M4E)", file_tag = "M4E",
    drone_enum = 99L, drone_sub_enum = 0L, payload_enum = 88L,
    payload_sub_enum = 0L, image_width = 5280L, image_height = 3956L,
    footprint_width_ratio = 17.3 / 12.3,
    footprint_height_ratio = 13.0 / 12.3,
    max_speed = 21, min_photo_interval = 1,
    photo_intervals = c(1, 2, 3, 5, 7, 10, 15, 20, 30, 60),
    camera_note = "4/3 wide camera, 20 MP (5280 x 3956)"
  ),
  m4t = list(
    label = "DJI Matrice 4 Thermal (M4T)", file_tag = "M4T",
    drone_enum = 99L, drone_sub_enum = 1L, payload_enum = 89L,
    payload_sub_enum = 0L, image_width = 8064L, image_height = 6048L,
    footprint_width_ratio = unname(m4t_footprint_ratios[["width"]]),
    footprint_height_ratio = unname(m4t_footprint_ratios[["height"]]),
    max_speed = 21, min_photo_interval = 1,
    photo_intervals = c(1, 2, 3, 5, 7, 10, 15, 20, 30, 60),
    camera_note = "1/1.3-inch wide camera, 48 MP (8064 x 6048)"
  )
)

optimize_capture_settings <- function(camera, altitude_m, front_overlap) {
  footprint_height_m <- camera$footprint_height_ratio * altitude_m
  target_spacing_m <- footprint_height_m * (1 - front_overlap / 100)
  min_interval <- camera$min_photo_interval
  if (is.null(min_interval) || !is.finite(min_interval)) {
    min_interval <- min(camera$photo_intervals)
  }

  # Fly as fast as the camera can sustain while retaining the requested
  # spacing. WPML timed triggers accept floating-point seconds.
  speed <- min(camera$max_speed, target_spacing_m / min_interval)
  # Round down so the camera's minimum interval is never exceeded after
  # converting speed to the 0.1 m/s precision used by DJI Pilot 2.
  speed <- max(0.1, floor(speed * 10) / 10)
  interval <- max(min_interval, round(target_spacing_m / speed, 2))
  actual_spacing_m <- speed * interval

  list(
    speed = speed,
    interval = interval,
    target_spacing_m = target_spacing_m,
    actual_spacing_m = actual_spacing_m,
    achieved_overlap = 100 * (1 - actual_spacing_m / footprint_height_m)
  )
}

# Litchi Pilot supports the selected enterprise aircraft, but its waypoint files
# still need camera-specific capture values. Reject a plan if a later code change
# attempts to export a speed or interval outside the selected wide-camera profile.
validate_model_capture_settings <- function(camera, speed, photo_interval,
                                            export_name = "Mission") {
  if (is.null(camera$label) || is.null(camera$max_speed) ||
      is.null(camera$photo_intervals)) {
    stop(paste0(export_name, " requires a supported aircraft profile."))
  }
  if (!is.finite(speed) || speed <= 0 || speed > camera$max_speed + 1e-9) {
    stop(sprintf(
      "%s speed must be greater than 0 and no more than %.1f m/s for %s.",
      export_name, camera$max_speed, camera$label
    ))
  }
  min_interval <- camera$min_photo_interval
  if (is.null(min_interval) || !is.finite(min_interval)) {
    min_interval <- min(camera$photo_intervals)
  }
  if (!is.finite(photo_interval) || photo_interval < min_interval - 1e-9) {
    stop(sprintf(
      "%s photo interval %.2f s is below the %.2f s minimum for the selected %s profile.",
      export_name, photo_interval, min_interval, camera$file_tag
    ))
  }
  invisible(TRUE)
}

# Build the zero-based waypoint records used by both DJI WPML files. Each
# transect retains its own start/end indexes so interval actions never include
# the connector leg to the next transect.
build_dji_waypoint_records <- function(transects) {
  waypoints <- list()
  transect_ranges <- vector("list", length(transects))
  waypoint_idx <- 0L

  for (i in seq_along(transects)) {
    transect <- transects[[i]]
    if (is.null(transect) || nrow(transect) < 2) next
    start_idx <- waypoint_idx

    for (j in seq_len(nrow(transect))) {
      waypoints[[length(waypoints) + 1L]] <- list(
        index = waypoint_idx,
        lng = transect[j, 1],
        lat = transect[j, 2],
        transect = i,
        is_transect_start = (j == 1L),
        is_transect_end = (j == nrow(transect))
      )
      waypoint_idx <- waypoint_idx + 1L
    }

    transect_ranges[[i]] <- c(start = start_idx, end = waypoint_idx - 1L)
  }

  transect_ranges <- Filter(Negate(is.null), transect_ranges)
  if (length(waypoints) < 2L || length(transect_ranges) < 1L) {
    stop("At least one valid survey transect is required for DJI export.")
  }
  list(waypoints = waypoints, transect_ranges = transect_ranges)
}

# DJI WPML Common Elements specifies multipleTiming + takePhoto for repeated
# interval capture. The start/end indexes delimit one transect only.
generate_dji_interval_action_group <- function(group_id, start_index, end_index,
                                               photo_interval) {
  sprintf('
        <wpml:actionGroup>
          <wpml:actionGroupId>%d</wpml:actionGroupId>
          <wpml:actionGroupStartIndex>%d</wpml:actionGroupStartIndex>
          <wpml:actionGroupEndIndex>%d</wpml:actionGroupEndIndex>
          <wpml:actionGroupMode>sequence</wpml:actionGroupMode>
          <wpml:actionTrigger>
            <wpml:actionTriggerType>multipleTiming</wpml:actionTriggerType>
            <wpml:actionTriggerParam>%.2f</wpml:actionTriggerParam>
          </wpml:actionTrigger>
          <wpml:action>
            <wpml:actionId>0</wpml:actionId>
            <wpml:actionActuatorFunc>takePhoto</wpml:actionActuatorFunc>
            <wpml:actionActuatorFuncParam>
              <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
              <wpml:useGlobalPayloadLensIndex>1</wpml:useGlobalPayloadLensIndex>
            </wpml:actionActuatorFuncParam>
          </wpml:action>
        </wpml:actionGroup>',
    as.integer(group_id), as.integer(start_index), as.integer(end_index),
    photo_interval
  )
}

# Generate the editable waypoint template. This intentionally does not use a
# mapping2d template because DJI mapping missions reject zero-percent sidelap.
generate_template_kml <- function(transects, altitude, speed, photo_interval,
                                  drone_model) {
  if (is.null(drone_model$drone_enum) || is.null(drone_model$payload_enum)) {
    stop("A supported DJI drone model is required for KMZ export.")
  }
  records <- build_dji_waypoint_records(transects)
  waypoints <- records$waypoints
  ranges <- records$transect_ranges

  waypoint_xmls <- lapply(seq_along(waypoints), function(i) {
    wp <- waypoints[[i]]
    range_match <- which(vapply(ranges, function(x) x[["start"]] == wp$index, logical(1)))
    actions <- if (length(range_match) == 1L) {
      range <- ranges[[range_match]]
      generate_dji_interval_action_group(range_match - 1L, range[["start"]],
                                         range[["end"]], photo_interval)
    } else ""

    sprintf('      <Placemark>
        <Point><coordinates>%.10f,%.10f</coordinates></Point>
        <wpml:index>%d</wpml:index>
        <wpml:ellipsoidHeight>%.6f</wpml:ellipsoidHeight>
        <wpml:height>%.6f</wpml:height>
        <wpml:useGlobalHeight>1</wpml:useGlobalHeight>
        <wpml:useGlobalSpeed>1</wpml:useGlobalSpeed>
        <wpml:useGlobalHeadingParam>1</wpml:useGlobalHeadingParam>
        <wpml:useGlobalTurnParam>1</wpml:useGlobalTurnParam>
        <wpml:gimbalPitchAngle>-90</wpml:gimbalPitchAngle>%s
      </Placemark>', wp$lng, wp$lat, wp$index, altitude, altitude, actions)
  })
  waypoints_str <- paste(unlist(waypoint_xmls), collapse = "\n")

  timestamp_ms <- sprintf("%.0f", as.numeric(Sys.time()) * 1000)

  sprintf('<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:wpml="http://www.dji.com/wpmz/1.0.6">
  <Document>
    <wpml:author>Megafauna Drone Survey Planner</wpml:author>
    <wpml:createTime>%s</wpml:createTime>
    <wpml:updateTime>%s</wpml:updateTime>
    <wpml:missionConfig>
      <wpml:flyToWaylineMode>safely</wpml:flyToWaylineMode>
      <wpml:finishAction>goHome</wpml:finishAction>
      <wpml:exitOnRCLost>goContinue</wpml:exitOnRCLost>
      <wpml:executeRCLostAction>goBack</wpml:executeRCLostAction>
      <wpml:takeOffSecurityHeight>20</wpml:takeOffSecurityHeight>
      <wpml:globalTransitionalSpeed>15</wpml:globalTransitionalSpeed>
      <wpml:globalRTHHeight>100</wpml:globalRTHHeight>
      <wpml:droneInfo>
        <wpml:droneEnumValue>%d</wpml:droneEnumValue>
        <wpml:droneSubEnumValue>%d</wpml:droneSubEnumValue>
      </wpml:droneInfo>
      <wpml:waylineAvoidLimitAreaMode>0</wpml:waylineAvoidLimitAreaMode>
      <wpml:payloadInfo>
        <wpml:payloadEnumValue>%d</wpml:payloadEnumValue>
        <wpml:payloadSubEnumValue>%d</wpml:payloadSubEnumValue>
        <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
      </wpml:payloadInfo>
    </wpml:missionConfig>
    <Folder>
      <wpml:templateType>waypoint</wpml:templateType>
      <wpml:templateId>0</wpml:templateId>
      <wpml:waylineCoordinateSysParam>
        <wpml:coordinateMode>WGS84</wpml:coordinateMode>
        <wpml:heightMode>relativeToStartPoint</wpml:heightMode>
        <wpml:globalShootHeight>%.6f</wpml:globalShootHeight>
        <wpml:positioningType>GPS</wpml:positioningType>
        <wpml:surfaceFollowModeEnable>0</wpml:surfaceFollowModeEnable>
      </wpml:waylineCoordinateSysParam>
      <wpml:autoFlightSpeed>%.1f</wpml:autoFlightSpeed>
      <wpml:gimbalPitchMode>usePointSetting</wpml:gimbalPitchMode>
      <wpml:globalWaypointHeadingParam>
        <wpml:waypointHeadingMode>followWayline</wpml:waypointHeadingMode>
        <wpml:waypointHeadingAngle>0</wpml:waypointHeadingAngle>
        <wpml:waypointPoiPoint>0.000000,0.000000,0.000000</wpml:waypointPoiPoint>
        <wpml:waypointHeadingPathMode>followBadArc</wpml:waypointHeadingPathMode>
      </wpml:globalWaypointHeadingParam>
      <wpml:globalWaypointTurnMode>toPointAndStopWithDiscontinuityCurvature</wpml:globalWaypointTurnMode>
      <wpml:globalUseStraightLine>1</wpml:globalUseStraightLine>
      <wpml:globalHeight>%.6f</wpml:globalHeight>
%s
      <wpml:payloadParam>
        <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
        <wpml:focusMode>firstPoint</wpml:focusMode>
        <wpml:meteringMode>average</wpml:meteringMode>
        <wpml:returnMode>singleReturnStrongest</wpml:returnMode>
        <wpml:samplingRate>240000</wpml:samplingRate>
        <wpml:scanningMode>repetitive</wpml:scanningMode>
        <wpml:imageFormat>wide</wpml:imageFormat>
        <wpml:photoSize>default_l</wpml:photoSize>
      </wpml:payloadParam>
    </Folder>
  </Document>
</kml>',
    timestamp_ms,
    timestamp_ms,
    drone_model$drone_enum,
    drone_model$drone_sub_enum,
    drone_model$payload_enum,
    drone_model$payload_sub_enum,
    altitude,
    speed,
    altitude,
    waypoints_str
  )
}

# Calculate optimal transect direction to minimize number of transects
calculate_optimal_direction <- function(poly_df) {
  if (is.null(poly_df) || nrow(poly_df) < 3) return(0)

  coordinate_names <- if (all(c("lng", "lat") %in% names(poly_df))) {
    c("lng", "lat")
  } else {
    names(poly_df)[1:2]
  }
  polygon_matrix <- as.matrix(poly_df[, coordinate_names, drop = FALSE])
  storage.mode(polygon_matrix) <- "double"
  if (!all(polygon_matrix[1, ] == polygon_matrix[nrow(polygon_matrix), ])) {
    polygon_matrix <- rbind(polygon_matrix, polygon_matrix[1, ])
  }
  polygon_ll <- sf::st_sfc(sf::st_polygon(list(polygon_matrix)), crs = 4326)
  centre <- colMeans(polygon_matrix[-nrow(polygon_matrix), , drop = FALSE])
  utm_zone <- max(1, min(60, floor((centre[[1]] + 180) / 6) + 1))
  utm_epsg <- if (centre[[2]] >= 0) 32600 + utm_zone else 32700 + utm_zone
  polygon_xy <- sf::st_coordinates(sf::st_transform(polygon_ll, utm_epsg))

  # Match the generator's bearing convention and metre-based cross-track vector.
  angles <- seq(0, 179, by = 1)
  widths <- vapply(angles, function(angle) {
    theta <- angle * pi / 180
    cross_track <- polygon_xy[, "X"] * (-cos(theta)) +
      polygon_xy[, "Y"] * sin(theta)
    diff(range(cross_track))
  }, numeric(1))
  angles[[which.min(widths)]]
}

validate_aoi_polygon <- function(poly_df) {
  if (is.null(poly_df) || nrow(poly_df) < 3) {
    stop("The survey boundary needs at least three vertices.")
  }
  coordinate_names <- if (all(c("lng", "lat") %in% names(poly_df))) {
    c("lng", "lat")
  } else {
    names(poly_df)[1:2]
  }
  coordinates <- as.matrix(poly_df[, coordinate_names, drop = FALSE])
  storage.mode(coordinates) <- "double"
  if (any(!is.finite(coordinates)) || any(abs(coordinates[, 1]) > 180) ||
      any(abs(coordinates[, 2]) > 90)) {
    stop("The survey boundary contains invalid longitude or latitude values.")
  }
  if (nrow(unique(coordinates)) < 3) {
    stop("The survey boundary needs at least three distinct vertices.")
  }
  if (!all(coordinates[1, ] == coordinates[nrow(coordinates), ])) {
    coordinates <- rbind(coordinates, coordinates[1, ])
  }
  geometry <- sf::st_sfc(sf::st_polygon(list(coordinates)), crs = 4326)
  valid <- suppressWarnings(sf::st_is_valid(geometry, reason = TRUE))
  if (length(valid) != 1L || is.na(valid) || !identical(valid, "Valid Geometry")) {
    reason <- if (length(valid) == 1L && !is.na(valid)) valid else "invalid geometry"
    stop("The survey boundary is invalid: ", reason, ".")
  }
  if (!is.finite(abs(geosphere::areaPolygon(coordinates))) ||
      abs(geosphere::areaPolygon(coordinates)) <= 0) {
    stop("The survey boundary has no usable area.")
  }
  invisible(TRUE)
}

# Fit a minimum rotated rectangle to a rectangle-like AOI and return the
# bearings of its long and short edges. Bearings are clockwise from north and
# normalized to the flight-planner's 0-179 degree range.
calculate_polygon_alignment <- function(poly_df) {
  if (is.null(poly_df) || nrow(poly_df) < 3) {
    stop("At least three polygon vertices are required.")
  }

  coordinate_names <- if (all(c("lng", "lat") %in% names(poly_df))) {
    c("lng", "lat")
  } else {
    names(poly_df)[1:2]
  }
  polygon_matrix <- as.matrix(poly_df[, coordinate_names, drop = FALSE])
  storage.mode(polygon_matrix) <- "double"
  if (!all(polygon_matrix[1, ] == polygon_matrix[nrow(polygon_matrix), ])) {
    polygon_matrix <- rbind(polygon_matrix, polygon_matrix[1, ])
  }

  polygon_ll <- sf::st_sfc(sf::st_polygon(list(polygon_matrix)), crs = 4326)
  center <- sf::st_coordinates(sf::st_centroid(polygon_ll))[1, ]
  utm_zone <- max(1, min(60, floor((center[["X"]] + 180) / 6) + 1))
  utm_epsg <- if (center[["Y"]] >= 0) 32600 + utm_zone else 32700 + utm_zone
  polygon_utm <- sf::st_transform(polygon_ll, utm_epsg)
  rectangle <- sf::st_minimum_rotated_rectangle(polygon_utm)
  rectangle_coords <- sf::st_coordinates(rectangle)[, c("X", "Y"), drop = FALSE]

  edge_vectors <- rectangle_coords[-1, , drop = FALSE] -
    rectangle_coords[-nrow(rectangle_coords), , drop = FALSE]
  edge_lengths <- sqrt(rowSums(edge_vectors^2))
  valid_edges <- which(edge_lengths > sqrt(.Machine$double.eps))
  if (length(valid_edges) < 2) stop("Could not determine polygon edge directions.")

  edge_bearing <- function(vector) {
    bearing <- (atan2(vector[[1]], vector[[2]]) * 180 / pi) %% 180
    if (abs(bearing - 180) < 1e-8 || abs(bearing) < 1e-8) 0 else bearing
  }
  long_index <- valid_edges[which.max(edge_lengths[valid_edges])]
  short_index <- valid_edges[which.min(edge_lengths[valid_edges])]

  c(
    long = edge_bearing(edge_vectors[long_index, ]),
    short = edge_bearing(edge_vectors[short_index, ])
  )
}

# Resize an AOI to a requested area using an inward (negative) or outward
# (positive) buffer in local UTM metres. For an inward buffer that splits a
# concave AOI, retain and solve against the largest resulting polygon because
# the flight planner supports one contiguous exterior ring.
resize_polygon_to_target_area <- function(poly_df, target_area_m2) {
  if (is.null(poly_df) || nrow(poly_df) < 3) {
    stop("Draw or import a polygon before resizing it.")
  }
  if (!is.numeric(target_area_m2) || length(target_area_m2) != 1 ||
      !is.finite(target_area_m2) || target_area_m2 <= 0) {
    stop("Target area must be a positive number.")
  }

  coordinate_names <- if (all(c("lng", "lat") %in% names(poly_df))) {
    c("lng", "lat")
  } else {
    names(poly_df)[1:2]
  }
  polygon_matrix <- as.matrix(poly_df[, coordinate_names, drop = FALSE])
  storage.mode(polygon_matrix) <- "double"
  if (!all(polygon_matrix[1, ] == polygon_matrix[nrow(polygon_matrix), ])) {
    polygon_matrix <- rbind(polygon_matrix, polygon_matrix[1, ])
  }

  mean_lon <- mean(polygon_matrix[, 1])
  mean_lat <- mean(polygon_matrix[, 2])
  utm_zone <- max(1, min(60, floor((mean_lon + 180) / 6) + 1))
  utm_epsg <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
  polygon_ll <- sf::st_sfc(sf::st_polygon(list(polygon_matrix)), crs = 4326)
  polygon_utm <- sf::st_transform(sf::st_make_valid(polygon_ll), utm_epsg)

  largest_polygon <- function(geometry) {
    if (length(geometry) == 0 || all(sf::st_is_empty(geometry))) return(NULL)
    polygons <- suppressWarnings(sf::st_collection_extract(sf::st_make_valid(geometry), "POLYGON"))
    polygons <- suppressWarnings(sf::st_cast(polygons, "POLYGON"))
    if (length(polygons) == 0 || all(sf::st_is_empty(polygons))) return(NULL)
    polygon_areas <- as.numeric(sf::st_area(polygons))
    polygons[which.max(polygon_areas)]
  }

  displayed_polygon_area <- function(geometry) {
    if (is.null(geometry)) return(0)
    geometry_ll <- sf::st_transform(geometry, 4326)
    coords <- sf::st_coordinates(geometry_ll)
    if ("L1" %in% colnames(coords)) {
      coords <- coords[coords[, "L1"] == 1, , drop = FALSE]
    }
    geosphere::areaPolygon(coords[, c("X", "Y"), drop = FALSE])
  }

  polygon_utm <- largest_polygon(polygon_utm)
  if (is.null(polygon_utm)) stop("The polygon geometry is invalid.")
  original_area_m2 <- displayed_polygon_area(polygon_utm)
  if (abs(original_area_m2 - target_area_m2) <= max(0.01, target_area_m2 * 1e-8)) {
    return(list(
      polygon = poly_df[, coordinate_names, drop = FALSE],
      buffer_m = 0,
      original_area_m2 = original_area_m2,
      achieved_area_m2 = original_area_m2
    ))
  }

  buffered_result <- function(distance_m) {
    geometry <- suppressWarnings(sf::st_buffer(
      polygon_utm,
      dist = distance_m,
      joinStyle = "MITRE",
      mitreLimit = 10
    ))
    geometry <- largest_polygon(geometry)
    list(
      geometry = geometry,
      area = displayed_polygon_area(geometry)
    )
  }

  initial_step <- max(1, sqrt(original_area_m2) / 20)
  if (target_area_m2 > original_area_m2) {
    lower <- 0
    upper <- initial_step
    for (iteration in seq_len(60)) {
      if (buffered_result(upper)$area >= target_area_m2) break
      upper <- upper * 2
    }
  } else {
    upper <- 0
    lower <- -initial_step
    for (iteration in seq_len(60)) {
      if (buffered_result(lower)$area <= target_area_m2) break
      lower <- lower * 2
    }
  }

  # Maintain area(lower) <= target <= area(upper).
  for (iteration in seq_len(60)) {
    midpoint <- (lower + upper) / 2
    if (buffered_result(midpoint)$area < target_area_m2) {
      lower <- midpoint
    } else {
      upper <- midpoint
    }
  }
  lower_result <- buffered_result(lower)
  upper_result <- buffered_result(upper)
  if (is.null(lower_result$geometry) ||
      abs(upper_result$area - target_area_m2) <= abs(lower_result$area - target_area_m2)) {
    selected_distance <- upper
    selected_result <- upper_result
  } else {
    selected_distance <- lower
    selected_result <- lower_result
  }
  if (is.null(selected_result$geometry)) stop("The requested inward buffer removes the entire polygon.")

  resized_ll <- sf::st_transform(selected_result$geometry, 4326)
  resized_coords <- sf::st_coordinates(resized_ll)
  if ("L1" %in% colnames(resized_coords)) {
    resized_coords <- resized_coords[resized_coords[, "L1"] == 1, , drop = FALSE]
  }
  resized_df <- data.frame(
    lng = as.numeric(resized_coords[, "X"]),
    lat = as.numeric(resized_coords[, "Y"])
  )
  if (nrow(resized_df) > 1 &&
      all(abs(resized_df[1, ] - resized_df[nrow(resized_df), ]) < 1e-10)) {
    resized_df <- resized_df[-nrow(resized_df), , drop = FALSE]
  }
  validate_aoi_polygon(resized_df)

  list(
    polygon = resized_df,
    buffer_m = selected_distance,
    original_area_m2 = original_area_m2,
    achieved_area_m2 = selected_result$area
  )
}

# Rotate an AOI around its area centroid in a local metre-based projection.
# Positive angles are clockwise to match bearings used elsewhere in the app.
rotate_polygon_about_center <- function(poly_df, angle_degrees) {
  if (is.null(poly_df) || nrow(poly_df) < 3) {
    stop("Draw or import a polygon before rotating it.")
  }
  if (!is.numeric(angle_degrees) || length(angle_degrees) != 1 ||
      !is.finite(angle_degrees)) {
    stop("Rotation angle must be a finite number.")
  }

  coordinate_names <- if (all(c("lng", "lat") %in% names(poly_df))) {
    c("lng", "lat")
  } else {
    names(poly_df)[1:2]
  }
  polygon_matrix <- as.matrix(poly_df[, coordinate_names, drop = FALSE])
  storage.mode(polygon_matrix) <- "double"
  if (!all(is.finite(polygon_matrix))) stop("Polygon coordinates must be finite.")
  if (!all(polygon_matrix[1, ] == polygon_matrix[nrow(polygon_matrix), ])) {
    polygon_matrix <- rbind(polygon_matrix, polygon_matrix[1, ])
  }

  normalized_angle <- angle_degrees %% 360
  if (abs(normalized_angle) < 1e-10) {
    return(data.frame(
      lng = as.numeric(polygon_matrix[-nrow(polygon_matrix), 1]),
      lat = as.numeric(polygon_matrix[-nrow(polygon_matrix), 2])
    ))
  }

  mean_lon <- mean(polygon_matrix[, 1])
  mean_lat <- mean(polygon_matrix[, 2])
  utm_zone <- max(1, min(60, floor((mean_lon + 180) / 6) + 1))
  utm_epsg <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
  polygon_ll <- sf::st_sfc(sf::st_polygon(list(polygon_matrix)), crs = 4326)
  polygon_utm <- sf::st_transform(polygon_ll, utm_epsg)
  center <- sf::st_coordinates(sf::st_centroid(polygon_utm))[1, c("X", "Y")]
  coords_utm <- sf::st_coordinates(polygon_utm)[, c("X", "Y"), drop = FALSE]

  theta <- normalized_angle * pi / 180
  clockwise_rotation <- matrix(
    c(cos(theta), -sin(theta), sin(theta), cos(theta)),
    nrow = 2,
    byrow = TRUE
  )
  centered <- sweep(coords_utm, 2, center, "-")
  rotated_coords <- centered %*% clockwise_rotation
  rotated_coords <- sweep(rotated_coords, 2, center, "+")
  rotated_coords[nrow(rotated_coords), ] <- rotated_coords[1, ]

  rotated_utm <- sf::st_sfc(sf::st_polygon(list(rotated_coords)), crs = utm_epsg)
  rotated_ll <- sf::st_transform(rotated_utm, 4326)
  result_coords <- sf::st_coordinates(rotated_ll)[, c("X", "Y"), drop = FALSE]
  result_coords <- result_coords[-nrow(result_coords), , drop = FALSE]
  data.frame(
    lng = as.numeric(result_coords[, "X"]),
    lat = as.numeric(result_coords[, "Y"])
  )
}

# Reorder transects into a continuous boustrophedon route. Calculations are
# performed in local UTM metres: longitude/latitude sorting and latitude-only
# endpoint tests fail for oblique flight directions and create crossing links.
reorder_transects_by_start <- function(transects, start_corner, transect_angle = 0) {
  if (length(transects) == 0) return(transects)

  all_ll <- do.call(rbind, lapply(transects, function(t) t[, 1:2, drop = FALSE]))
  mean_lon <- mean(all_ll[, 1])
  mean_lat <- mean(all_ll[, 2])
  utm_zone <- max(1, min(60, floor((mean_lon + 180) / 6) + 1))
  utm_epsg <- if (mean_lat >= 0) 32600 + utm_zone else 32700 + utm_zone

  transect_lines <- sf::st_sfc(
    lapply(transects, function(t) {
      sf::st_linestring(as.matrix(t[, 1:2, drop = FALSE]))
    }),
    crs = 4326
  )
  transect_lines_utm <- sf::st_transform(transect_lines, utm_epsg)
  transects_utm <- lapply(seq_along(transect_lines_utm), function(i) {
    sf::st_coordinates(transect_lines_utm[i])[, 1:2, drop = FALSE]
  })

  theta <- transect_angle * pi / 180
  along_vector <- c(sin(theta), cos(theta))
  cross_vector <- c(-cos(theta), sin(theta))
  project_on <- function(coords, vector) as.vector(coords %*% vector)

  cross_positions <- vapply(
    transects_utm,
    function(t) mean(project_on(t, cross_vector)),
    numeric(1)
  )
  along_positions <- vapply(
    transects_utm,
    function(t) mean(project_on(t, along_vector)),
    numeric(1)
  )

  # MULTILINESTRING intersections from concave AOIs can create several
  # components on one grid line. Keep those components together.
  sorted_indices <- order(cross_positions)
  sorted_cross <- cross_positions[sorted_indices]
  group_number <- cumsum(c(TRUE, diff(sorted_cross) > 0.05))
  line_groups <- unname(split(sorted_indices, group_number))

  bounds <- apply(do.call(rbind, transects_utm), 2, range)
  start_corner <- if (start_corner %in% c(
    "top_left", "top_right", "bottom_left", "bottom_right"
  )) start_corner else "top_right"
  target_corner <- c(
    if (grepl("left$", start_corner)) bounds[1, 1] else bounds[2, 1],
    if (grepl("^top", start_corner)) bounds[2, 2] else bounds[1, 2]
  )
  distance_sq <- function(a, b) sum((a - b)^2)

  group_distance_to_corner <- function(indices) {
    endpoints <- do.call(rbind, lapply(indices, function(i) {
      t <- transects_utm[[i]]
      rbind(t[1, ], t[nrow(t), ])
    }))
    min(rowSums((endpoints - matrix(target_corner, nrow(endpoints), 2, byrow = TRUE))^2))
  }

  # Start at whichever cross-track edge is closest to the requested map corner.
  if (length(line_groups) > 1 &&
      group_distance_to_corner(line_groups[[length(line_groups)]]) <
        group_distance_to_corner(line_groups[[1]])) {
    line_groups <- rev(line_groups)
  }

  build_group_route <- function(indices, increasing) {
    indices <- indices[order(along_positions[indices], decreasing = !increasing)]
    lapply(indices, function(index) {
      coords <- transects_utm[[index]]
      endpoint_along <- project_on(coords[c(1, nrow(coords)), , drop = FALSE], along_vector)
      reverse_segment <- if (increasing) endpoint_along[1] > endpoint_along[2] else endpoint_along[1] < endpoint_along[2]
      list(index = index, reverse = reverse_segment)
    })
  }

  route_endpoint <- function(route, first = TRUE) {
    item <- route[[if (first) 1 else length(route)]]
    coords <- transects_utm[[item$index]]
    if (item$reverse) coords <- coords[nrow(coords):1, , drop = FALSE]
    coords[if (first) 1 else nrow(coords), ]
  }

  ordered_route <- list()
  previous_end <- NULL
  for (group_index in seq_along(line_groups)) {
    forward <- build_group_route(line_groups[[group_index]], TRUE)
    reverse <- build_group_route(line_groups[[group_index]], FALSE)
    target <- if (is.null(previous_end)) target_corner else previous_end
    chosen <- if (distance_sq(route_endpoint(forward), target) <=
                  distance_sq(route_endpoint(reverse), target)) forward else reverse
    ordered_route <- c(ordered_route, chosen)
    previous_end <- route_endpoint(chosen, first = FALSE)
  }

  lapply(ordered_route, function(item) {
    transect <- transects[[item$index]]
    if (item$reverse) transect[nrow(transect):1, , drop = FALSE] else transect
  })
}

# Reverse the complete route, not just the list of lines. Reversing the point
# direction within each transect preserves the original short connector legs.
apply_reverse_transect_order <- function(transects, reverse_order = FALSE) {
  if (!isTRUE(reverse_order) || length(transects) == 0) return(transects)
  lapply(rev(transects), function(transect) {
    transect[nrow(transect):1, , drop = FALSE]
  })
}

transect_length_m <- function(coords) {
  coords <- as.matrix(coords[, 1:2, drop = FALSE])
  if (nrow(coords) < 2) return(0)
  sum(geosphere::distGeo(
    coords[-nrow(coords), , drop = FALSE],
    coords[-1, , drop = FALSE]
  ))
}

point_along_transect <- function(coords, distance_m) {
  coords <- as.matrix(coords[, 1:2, drop = FALSE])
  segment_lengths <- geosphere::distGeo(
    coords[-nrow(coords), , drop = FALSE],
    coords[-1, , drop = FALSE]
  )
  cumulative <- c(0, cumsum(segment_lengths))
  distance_m <- max(0, min(distance_m, cumulative[[length(cumulative)]]))
  index <- max(which(cumulative <= distance_m + 1e-7))
  if (index >= nrow(coords) || abs(distance_m - cumulative[[index]]) <= 1e-7) {
    return(unname(as.numeric(coords[index, ])))
  }
  unname(as.numeric(geosphere::destPoint(
    coords[index, ],
    geosphere::bearing(coords[index, ], coords[index + 1, ]),
    distance_m - cumulative[[index]]
  )))
}

# Shorten a line equally at both ends so the retained survey coverage remains
# centred within the drawn AOI.
trim_transect_to_length <- function(coords, max_length_m) {
  coords <- as.matrix(coords[, 1:2, drop = FALSE])
  total_length_m <- transect_length_m(coords)
  if (!is.finite(max_length_m) || max_length_m <= 0 ||
      total_length_m <= max_length_m) return(coords)

  segment_lengths <- geosphere::distGeo(
    coords[-nrow(coords), , drop = FALSE],
    coords[-1, , drop = FALSE]
  )
  cumulative <- c(0, cumsum(segment_lengths))
  start_distance_m <- (total_length_m - max_length_m) / 2
  end_distance_m <- start_distance_m + max_length_m
  internal <- which(
    cumulative > start_distance_m + 1e-7 &
      cumulative < end_distance_m - 1e-7
  )
  trimmed <- rbind(
    point_along_transect(coords, start_distance_m),
    if (length(internal) > 0) coords[internal, , drop = FALSE] else NULL,
    point_along_transect(coords, end_distance_m)
  )
  colnames(trimmed) <- colnames(coords)
  trimmed
}

generate_photo_points_for_transects <- function(transects, spacing_m) {
  if (!is.finite(spacing_m) || spacing_m <= 0) return(list())
  lapply(transects, function(coords) {
    coords <- as.matrix(coords[, 1:2, drop = FALSE])
    line_length_m <- transect_length_m(coords)
    distances <- seq(0, line_length_m, by = spacing_m)
    centres <- t(vapply(
      distances,
      function(distance_m) point_along_transect(coords, distance_m),
      numeric(2)
    ))
    colnames(centres) <- colnames(coords)
    attr(centres, "bearing") <- geosphere::bearing(
      coords[1, ], coords[nrow(coords), ]
    ) %% 360
    centres
  })
}

measure_transect_route <- function(transects) {
  transects <- Filter(function(x) !is.null(x) && nrow(x) >= 2, transects)
  if (length(transects) == 0) {
    return(list(
      transect_lengths_m = numeric(), survey_distance_m = 0,
      connector_distance_m = 0, route_distance_m = 0,
      longest_transect_m = 0
    ))
  }
  lengths <- vapply(transects, transect_length_m, numeric(1))
  connector_distance_m <- if (length(transects) > 1) {
    sum(vapply(seq_len(length(transects) - 1L), function(index) {
      current <- transects[[index]]
      next_line <- transects[[index + 1L]]
      geosphere::distGeo(
        current[nrow(current), 1:2], next_line[1, 1:2]
      )
    }, numeric(1)))
  } else 0
  list(
    transect_lengths_m = lengths,
    survey_distance_m = sum(lengths),
    connector_distance_m = connector_distance_m,
    route_distance_m = sum(lengths) + connector_distance_m,
    longest_transect_m = max(lengths)
  )
}

# Build an endpoint-based native Litchi plan. Photo interval is enabled on each
# transect's outgoing segments and explicitly disabled at the transect end, so
# connector legs never trigger interval photos.
build_litchi_native_plan <- function(transects, altitude, speed, photo_interval,
                                     drone_model = NULL) {
  if (length(transects) == 0) stop("At least one transect is required.")
  if (!is.finite(photo_interval) || photo_interval < 0.7) {
    stop("Photo interval must be at least 0.7 seconds.")
  }
  if (!is.null(drone_model)) {
    validate_model_capture_settings(
      drone_model, speed, photo_interval, "Litchi export"
    )
  }

  plan_rows <- list()
  for (transect in transects) {
    if (nrow(transect) < 2) next
    heading <- geosphere::bearing(
      as.numeric(transect[1, 1:2]),
      as.numeric(transect[nrow(transect), 1:2])
    ) %% 360
    interval_values <- rep(photo_interval, nrow(transect))
    interval_values[nrow(transect)] <- 0

    plan_rows[[length(plan_rows) + 1]] <- data.frame(
      latitude = transect[, 2],
      longitude = transect[, 1],
      `altitude(m)` = altitude,
      `heading(deg)` = heading,
      `curvesize(m)` = 0.2,
      rotationdir = 0,
      gimbalmode = 2,
      gimbalpitchangle = -90,
      actiontype1 = -1,
      actionparam1 = 0,
      altitudemode = 0,
      `speed(m/s)` = speed,
      poi_latitude = 0,
      poi_longitude = 0,
      `poi_altitude(m)` = 0,
      poi_altitudemode = 0,
      photo_timeinterval = interval_values,
      photo_distinterval = 0,
      check.names = FALSE
    )
  }

  if (length(plan_rows) == 0) stop("No valid survey transects were generated.")
  do.call(rbind, plan_rows)
}

# Generate waylines.wpml content
generate_waylines_wpml <- function(transects, altitude, speed, photo_interval,
                                   drone_model) {
  if (is.null(drone_model$drone_enum) || is.null(drone_model$payload_enum)) {
    stop("A supported DJI drone model is required for KMZ export.")
  }
  records <- build_dji_waypoint_records(transects)
  waypoints <- records$waypoints
  ranges <- records$transect_ranges
  waypoint_coords <- do.call(rbind, lapply(waypoints, function(wp) c(wp$lng, wp$lat)))
  segment_lengths <- if (nrow(waypoint_coords) > 1) {
    geosphere::distHaversine(waypoint_coords[-nrow(waypoint_coords), , drop = FALSE],
                             waypoint_coords[-1, , drop = FALSE])
  } else {
    numeric(0)
  }
  mission_distance <- sum(segment_lengths)
  # DJI Pilot includes an estimated duration that accounts for nominal flight
  # time plus waypoint/turn overhead. This is an estimate for preview metadata;
  # it does not control execution timing.
  # Pilot 2 preview duration includes speed-dependent acceleration and turn
  # overhead beyond distance/speed. This linear per-waypoint model is fitted
  # to the three supplied M3E Pilot 2 reference missions.
  turn_overhead_per_waypoint <- max(0, 0.21293162 * speed - 0.50566326)
  mission_duration <- mission_distance / speed +
    turn_overhead_per_waypoint * length(waypoints)

  # Stop at every endpoint so the timed action range ends exactly at the end of
  # each transect before the aircraft starts the connector leg.
  for (i in seq_along(waypoints)) {
    waypoints[[i]]$turn_mode <- "toPointAndStopWithDiscontinuityCurvature"
    waypoints[[i]]$turn_dist <- 0

    if (i < length(waypoints)) {
      waypoints[[i]]$heading_angle <- geosphere::bearing(
        waypoint_coords[i, ], waypoint_coords[i + 1, ]
      )
      waypoints[[i]]$heading_enabled <- 1
    } else {
      waypoints[[i]]$heading_angle <- 0
      waypoints[[i]]$heading_enabled <- 0
    }
  }
  
  # Generate waypoint XML strings
  waypoint_xmls <- sapply(waypoints, function(wp) {
    range_match <- which(vapply(ranges, function(x) x[["start"]] == wp$index, logical(1)))
    actions <- if (length(range_match) == 1L) {
      range <- ranges[[range_match]]
      generate_dji_interval_action_group(range_match - 1L, range[["start"]],
                                         range[["end"]], photo_interval)
    } else ""
    
    sprintf('      <Placemark>
        <Point>
          <coordinates>
            %.10f,%.10f
          </coordinates>
        </Point>
        <wpml:index>%d</wpml:index>
        <wpml:executeHeight>%.6f</wpml:executeHeight>
        <wpml:waypointSpeed>%.1f</wpml:waypointSpeed>
        <wpml:waypointHeadingParam>
          <wpml:waypointHeadingMode>followWayline</wpml:waypointHeadingMode>
          <wpml:waypointHeadingAngle>%.6f</wpml:waypointHeadingAngle>
          <wpml:waypointPoiPoint>0.000000,0.000000,0.000000</wpml:waypointPoiPoint>
          <wpml:waypointHeadingAngleEnable>%d</wpml:waypointHeadingAngleEnable>
          <wpml:waypointHeadingPathMode>followBadArc</wpml:waypointHeadingPathMode>
          <wpml:waypointHeadingPoiIndex>0</wpml:waypointHeadingPoiIndex>
        </wpml:waypointHeadingParam>
        <wpml:waypointTurnParam>
          <wpml:waypointTurnMode>%s</wpml:waypointTurnMode>
          <wpml:waypointTurnDampingDist>%.6f</wpml:waypointTurnDampingDist>
        </wpml:waypointTurnParam>
        <wpml:useStraightLine>1</wpml:useStraightLine>%s
        <wpml:waypointGimbalHeadingParam>
          <wpml:waypointGimbalPitchAngle>-90</wpml:waypointGimbalPitchAngle>
          <wpml:waypointGimbalYawAngle>0</wpml:waypointGimbalYawAngle>
        </wpml:waypointGimbalHeadingParam>
        <wpml:isRisky>0</wpml:isRisky>
        <wpml:waypointWorkType>0</wpml:waypointWorkType>
      </Placemark>',
      wp$lng, wp$lat, wp$index, altitude, speed,
      wp$heading_angle, wp$heading_enabled,
      wp$turn_mode, wp$turn_dist, actions)
  })
  
  waypoints_str <- paste(waypoint_xmls, collapse = "\n")
  
  sprintf('<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:wpml="http://www.dji.com/wpmz/1.0.6">
  <Document>
    <wpml:missionConfig>
      <wpml:flyToWaylineMode>safely</wpml:flyToWaylineMode>
      <wpml:finishAction>goHome</wpml:finishAction>
      <wpml:exitOnRCLost>goContinue</wpml:exitOnRCLost>
      <wpml:executeRCLostAction>goBack</wpml:executeRCLostAction>
      <wpml:takeOffSecurityHeight>20</wpml:takeOffSecurityHeight>
      <wpml:globalTransitionalSpeed>15</wpml:globalTransitionalSpeed>
      <wpml:globalRTHHeight>100</wpml:globalRTHHeight>
      <wpml:droneInfo>
        <wpml:droneEnumValue>%d</wpml:droneEnumValue>
        <wpml:droneSubEnumValue>%d</wpml:droneSubEnumValue>
      </wpml:droneInfo>
      <wpml:waylineAvoidLimitAreaMode>0</wpml:waylineAvoidLimitAreaMode>
      <wpml:payloadInfo>
        <wpml:payloadEnumValue>%d</wpml:payloadEnumValue>
        <wpml:payloadSubEnumValue>%d</wpml:payloadSubEnumValue>
        <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
      </wpml:payloadInfo>
    </wpml:missionConfig>
    <Folder>
      <wpml:templateId>0</wpml:templateId>
      <wpml:executeHeightMode>relativeToStartPoint</wpml:executeHeightMode>
      <wpml:waylineId>0</wpml:waylineId>
      <wpml:distance>%.6f</wpml:distance>
      <wpml:duration>%.6f</wpml:duration>
      <wpml:autoFlightSpeed>%.1f</wpml:autoFlightSpeed>
      <wpml:startActionGroup>
        <wpml:action>
          <wpml:actionId>0</wpml:actionId>
          <wpml:actionActuatorFunc>gimbalRotate</wpml:actionActuatorFunc>
          <wpml:actionActuatorFuncParam>
            <wpml:gimbalHeadingYawBase>aircraft</wpml:gimbalHeadingYawBase>
            <wpml:gimbalRotateMode>absoluteAngle</wpml:gimbalRotateMode>
            <wpml:gimbalPitchRotateEnable>1</wpml:gimbalPitchRotateEnable>
            <wpml:gimbalPitchRotateAngle>-90</wpml:gimbalPitchRotateAngle>
            <wpml:gimbalRollRotateEnable>0</wpml:gimbalRollRotateEnable>
            <wpml:gimbalRollRotateAngle>0</wpml:gimbalRollRotateAngle>
            <wpml:gimbalYawRotateEnable>1</wpml:gimbalYawRotateEnable>
            <wpml:gimbalYawRotateAngle>0</wpml:gimbalYawRotateAngle>
            <wpml:gimbalRotateTimeEnable>0</wpml:gimbalRotateTimeEnable>
            <wpml:gimbalRotateTime>10</wpml:gimbalRotateTime>
            <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
          </wpml:actionActuatorFuncParam>
        </wpml:action>
        <wpml:action>
          <wpml:actionId>1</wpml:actionId>
          <wpml:actionActuatorFunc>hover</wpml:actionActuatorFunc>
          <wpml:actionActuatorFuncParam>
            <wpml:hoverTime>0.5</wpml:hoverTime>
          </wpml:actionActuatorFuncParam>
        </wpml:action>
        <wpml:action>
          <wpml:actionId>2</wpml:actionId>
          <wpml:actionActuatorFunc>setFocusType</wpml:actionActuatorFunc>
          <wpml:actionActuatorFuncParam>
            <wpml:cameraFocusType>manual</wpml:cameraFocusType>
            <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
          </wpml:actionActuatorFuncParam>
        </wpml:action>
        <wpml:action>
          <wpml:actionId>3</wpml:actionId>
          <wpml:actionActuatorFunc>focus</wpml:actionActuatorFunc>
          <wpml:actionActuatorFuncParam>
            <wpml:focusX>0</wpml:focusX>
            <wpml:focusY>0</wpml:focusY>
            <wpml:focusRegionWidth>0</wpml:focusRegionWidth>
            <wpml:focusRegionHeight>0</wpml:focusRegionHeight>
            <wpml:isPointFocus>0</wpml:isPointFocus>
            <wpml:isInfiniteFocus>1</wpml:isInfiniteFocus>
            <wpml:payloadPositionIndex>0</wpml:payloadPositionIndex>
            <wpml:isCalibrationFocus>0</wpml:isCalibrationFocus>
          </wpml:actionActuatorFuncParam>
        </wpml:action>
        <wpml:action>
          <wpml:actionId>4</wpml:actionId>
          <wpml:actionActuatorFunc>hover</wpml:actionActuatorFunc>
          <wpml:actionActuatorFuncParam>
            <wpml:hoverTime>1</wpml:hoverTime>
          </wpml:actionActuatorFuncParam>
        </wpml:action>
      </wpml:startActionGroup>
      <wpml:realTimeFollowSurfaceByFov>0</wpml:realTimeFollowSurfaceByFov>
%s
    </Folder>
  </Document>
</kml>',
    drone_model$drone_enum,
    drone_model$drone_sub_enum,
    drone_model$payload_enum,
    drone_model$payload_sub_enum,
    mission_distance,
    mission_duration,
    speed,
    waypoints_str
  )
}

# The native New Litchi Hub format is proprietary. This writer follows the
# version-15 LCH2 layout verified against the New Litchi Mission Hub client.
validate_litchi_native_plan <- function(plan, cruise_speed, drone_model = NULL) {
  required <- c(
    "latitude", "longitude", "altitude(m)", "heading(deg)",
    "curvesize(m)", "gimbalmode", "gimbalpitchangle", "actiontype1",
    "actionparam1", "altitudemode", "speed(m/s)", "photo_timeinterval",
    "photo_distinterval"
  )
  if (is.null(plan) || !is.data.frame(plan) || nrow(plan) < 1) {
    stop("A Litchi plan with at least one waypoint is required.")
  }
  if (!all(required %in% names(plan))) {
    stop("The Litchi plan is missing fields required by the native format.")
  }
  if (nrow(plan) > 65535) {
    stop("The native Litchi export supports at most 65,535 waypoints.")
  }
  numeric_values <- unlist(plan[required], use.names = FALSE)
  if (any(!is.finite(numeric_values)) || !is.finite(cruise_speed) || cruise_speed <= 0) {
    stop("The native Litchi export contains invalid numeric values.")
  }
  if (any(plan$latitude < -90 | plan$latitude > 90) ||
      any(plan$longitude < -180 | plan$longitude > 180)) {
    stop("Litchi waypoint coordinates fall outside valid WGS84 ranges.")
  }
  if (any(plan$photo_timeinterval < 0)) {
    stop("Megafauna native exports require non-negative interval values.")
  }
  active_intervals <- plan$photo_timeinterval[plan$photo_timeinterval > 0]
  if (length(active_intervals) == 0 || any(active_intervals < 0.7)) {
    stop("Megafauna native exports require valid interval capture on transects.")
  }
  if (!is.null(drone_model)) {
    if (any(abs(plan$`speed(m/s)` - cruise_speed) > 1e-6)) {
      stop("Litchi waypoint speeds must match the optimized mission speed.")
    }
    if (length(unique(round(active_intervals, 6))) != 1) {
      stop("Litchi transect legs must use one optimized photo interval.")
    }
    validate_model_capture_settings(
      drone_model, cruise_speed, active_intervals[[1]], "Litchi export"
    )
  }
  invisible(TRUE)
}

build_litchi_lch2 <- function(plan, cruise_speed, drone_model = NULL) {
  validate_litchi_native_plan(plan, cruise_speed, drone_model)

  pack_bin <- function(value, size, what = c("integer", "double")) {
    what <- match.arg(what)
    con <- rawConnection(raw(0), open = "wb")
    on.exit(close(con), add = TRUE)
    if (what == "integer") {
      writeBin(as.integer(value), con, size = size, endian = "big")
    } else {
      writeBin(as.numeric(value), con, size = size, endian = "big")
    }
    rawConnectionValue(con)
  }
  i16 <- function(x) pack_bin(x, 2, "integer")
  i32 <- function(x) pack_bin(x, 4, "integer")
  f32 <- function(x) pack_bin(x, 4, "double")
  f64 <- function(x) pack_bin(x, 8, "double")
  u8 <- function(x) as.raw(as.integer(x) %% 256L)
  boolean <- function(x) u8(if (isTRUE(x)) 1L else 0L)
  sized <- function(payload) c(i32(length(payload)), payload)
  object_block <- function(id, payload) c(i16(id), sized(payload))
  array_block <- function(id, objects) {
    payload <- c(i16(id), i32(length(objects)))
    for (object in objects) payload <- c(payload, sized(object))
    payload
  }

  elevation_unknown <- -1e5
  altitude <- as.numeric(plan$`altitude(m)`[1])
  settings <- c(
    i16(0), i16(0), u8(0), f32(altitude),
    f32(cruise_speed), f32(cruise_speed), u8(0), u8(0), f32(0),
    u8(3), f32(-90), u8(1), u8(0), f32(0), i16(0), i16(0),
    u8(0), f64(plan$latitude[1]), f64(plan$longitude[1]),
    f32(elevation_unknown), boolean(FALSE),
    u8(80), u8(0), boolean(FALSE), f32(0), f32(0), u8(0), u8(0),
    f32(50), u8(1), u8(0), u8(1), boolean(FALSE), boolean(FALSE),
    u8(2), f32(1), f64(0), f64(0), f64(0), f32(elevation_unknown),
    boolean(FALSE)
  )

  waypoint_objects <- lapply(seq_len(nrow(plan)), function(index) {
    actions <- if (plan$actiontype1[index] >= 0) {
      action <- c(i32(plan$actiontype1[index]), i32(plan$actionparam1[index]))
      c(i32(1), sized(action))
    } else {
      i32(0)
    }
    c(
      f64(plan$latitude[index]), f64(plan$longitude[index]),
      f32(plan$`altitude(m)`[index]), f32(plan$`altitude(m)`[index]),
      f32(elevation_unknown), boolean(FALSE), boolean(TRUE),
      f32(plan$`speed(m/s)`[index]), boolean(TRUE),
      u8(0), f32(plan$`curvesize(m)`[index]), boolean(TRUE),
      u8(0), f32(plan$`heading(deg)`[index]), u8(2), boolean(FALSE),
      u8(3), f32(plan$gimbalpitchangle[index]), boolean(TRUE),
      i32(-1), u8(0), f32(plan$photo_timeinterval[index]), boolean(TRUE), actions,
      u8(0), f32(0.9), f32(1)
    )
  })

  blocks <- list(
    object_block(1, settings),
    array_block(2, waypoint_objects),
    array_block(3, list()),
    array_block(4, list()),
    array_block(5, list())
  )
  output <- c(charToRaw("lch2"), i32(15), i32(length(blocks)))
  for (block in blocks) output <- c(output, sized(block))
  output
}

write_litchi_lchz <- function(file, plan, cruise_speed, drone_model = NULL) {
  lch2_data <- build_litchi_lch2(plan, cruise_speed, drone_model)
  mission_root <- tempfile("litchi_lchz_")
  dir.create(mission_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(mission_root, recursive = TRUE, force = TRUE), add = TRUE)

  lch2_path <- file.path(mission_root, "flight.lch2")
  lch2_con <- base::file(lch2_path, open = "wb")
  writeBin(lch2_data, lch2_con)
  close(lch2_con)

  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path, force = TRUE), add = TRUE)
  zip::zipr(
    zipfile = zip_path,
    files = "flight.lch2",
    root = mission_root,
    include_directories = FALSE
  )
  zip_data <- readBin(zip_path, what = "raw", n = file.info(zip_path)$size)

  con <- base::file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("lchz"), con)
  writeBin(as.integer(length(zip_data)), con, size = 4, endian = "big")
  writeBin(zip_data, con)
  invisible(file)
}

write_plan_summary_html <- function(file, plan_data) {
  payload <- jsonlite::toJSON(plan_data, auto_unbox = TRUE, digits = 10,
                              null = "null", na = "null")
  payload <- gsub("<", "\\\\u003c", payload, fixed = TRUE)
  template <- '<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Megafauna drone survey plan</title>
<style>
:root{--navy:#123047;--teal:#16857b;--orange:#e27a16;--red:#c7352b;--ink:#17232d;--muted:#60717e;--line:#d8e1e7;--paper:#fff;--wash:#f3f7f9}*{box-sizing:border-box}body{margin:0;color:var(--ink);background:var(--wash);font:15px/1.45 -apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif}header{padding:28px clamp(18px,4vw,54px);color:#fff;background:linear-gradient(125deg,var(--navy),#155f78)}header h1{margin:0 0 5px;font-size:clamp(25px,4vw,38px)}header p{margin:0;opacity:.88}main{max-width:1500px;margin:auto;padding:22px}.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(165px,1fr));gap:12px;margin-bottom:18px}.card,.panel{background:var(--paper);border:1px solid var(--line);border-radius:12px;box-shadow:0 2px 9px #18394d10}.card{padding:15px 17px}.label{color:var(--muted);font-size:12px;font-weight:700;letter-spacing:.055em;text-transform:uppercase}.value{margin-top:4px;color:var(--navy);font-size:25px;font-weight:760}.sub{margin-top:2px;color:var(--muted);font-size:12px}.layout{display:grid;grid-template-columns:minmax(0,2.35fr) minmax(280px,1fr);gap:16px}.panel{padding:16px;margin-bottom:16px}h2{margin:0 0 11px;font-size:19px}.toolbar{display:flex;flex-wrap:wrap;gap:13px;margin-bottom:10px;color:var(--muted);font-size:13px}.toolbar label{display:flex;gap:5px;align-items:center}button{border:1px solid #b8c7d0;border-radius:7px;padding:6px 10px;background:#fff;cursor:pointer}#map{width:100%;height:min(68vh,720px);min-height:430px;display:block;border:1px solid #c9d5dc;border-radius:8px;background:#edf3f4;cursor:grab;touch-action:none}.aoi{fill:#1570a618;stroke:#1570a6;stroke-width:2;vector-effect:non-scaling-stroke}.route{fill:none;stroke:#8b99a2;stroke-width:1.2;stroke-dasharray:5 4;vector-effect:non-scaling-stroke}.transect{stroke:var(--orange);stroke-width:2.6;vector-effect:non-scaling-stroke}.footprint{fill:#2e9b7238;stroke:#16857b;stroke-width:.55;vector-effect:non-scaling-stroke}.photo{fill:var(--red);stroke:#fff;stroke-width:.6;vector-effect:non-scaling-stroke}.start{fill:var(--red);stroke:#fff;stroke-width:2;vector-effect:non-scaling-stroke}.end-label{fill:var(--navy);font-weight:800;font-size:13px;paint-order:stroke;stroke:#fff;stroke-width:3px;vector-effect:non-scaling-stroke}.legend{display:flex;flex-wrap:wrap;gap:13px;margin-top:8px;color:var(--muted);font-size:12px}.swatch{display:inline-block;width:13px;height:13px;margin-right:5px;border-radius:3px;vertical-align:-2px}.kv{display:grid;grid-template-columns:1fr auto;gap:7px 12px;padding:6px 0;border-bottom:1px solid #edf1f3}.kv span:first-child{color:var(--muted)}.note{color:#43545f;font-size:13px}@media(max-width:900px){.layout{grid-template-columns:1fr}#map{height:58vh}}@media print{body{background:#fff}main{max-width:none}.panel,.card{box-shadow:none}#map{height:520px}button{display:none}}
</style></head><body><header><h1>Megafauna drone survey plan</h1><p id="subtitle"></p></header><main><section class="cards" id="cards"></section><div class="layout"><div><section class="panel"><h2>Planned survey route</h2><div class="toolbar"><label><input id="toggle-route" type="checkbox" checked> Route</label><label><input id="toggle-photos" type="checkbox" checked> Photo centres</label><label><input id="toggle-footprints" type="checkbox"> Image footprints</label><button id="reset-view">Reset view</button></div><svg id="map" role="img" aria-label="Map of planned survey transects and image footprints"></svg><div class="legend"><span><i class="swatch" style="background:#1570a633;border:1px solid #1570a6"></i>AOI</span><span><i class="swatch" style="background:#e27a16"></i>Transect</span><span><i class="swatch" style="background:#c7352b;border-radius:50%"></i>Photo centre</span><span><i class="swatch" style="background:#2e9b7260;border:1px solid #16857b"></i>Footprint</span></div><p class="note">Offline plan map. Scroll to zoom and drag to pan. Numbered labels mark transect ends.</p></section></div><aside><section class="panel"><h2>Mission settings</h2><div id="settings"></div></section><section class="panel"><h2>Planning notes</h2><div class="note" id="notes"></div></section></aside></div></main><script id="plan-data" type="application/json">__PAYLOAD__</script><script>
const d=JSON.parse(document.getElementById("plan-data").textContent),m=d.metrics,fmt=(v,n=1)=>v==null?"—":Number(v).toFixed(n),trimStatus=m.trim_transect_length_enabled?(m.trimmed_transect_count>0?`${m.trimmed_transect_count} oversized ${m.trimmed_transect_count===1?"transect":"transects"} trimmed to ${fmt(m.transect_length_limit_km,2)} km; coverage reduced.`:`Enabled; no transects required trimming.`):"Off";document.getElementById("subtitle").textContent=`${d.drone_label} • generated ${d.generated_at} • ${d.transects.length} transects`;const cards=[["Area",fmt(m.area_ha,2)+" ha","survey polygon"],["Height",fmt(m.altitude_m,1)+" m","above home"],["Speed",fmt(m.speed_mps,1)+" m/s",m.optimisation_label],["Photo interval",fmt(m.photo_interval_s,2)+" s",fmt(m.forward_overlap_pct,1)+"% forward overlap"],["Footprint",fmt(m.footprint_width_m,1)+" × "+fmt(m.footprint_length_m,1)+" m","cross-track × along-track"],["Footprint gap",fmt(m.footprint_gap_m,1)+" m","between adjacent footprints"],["Longest transect",fmt(m.longest_transect_km,2)+" km",trimStatus],["Photos",m.photo_count,"planned centres"],["Route",fmt(m.route_distance_km,2)+" km",fmt(m.flight_time_min,1)+" min • "+m.batteries+" battery estimate"]];document.getElementById("cards").innerHTML=cards.map(c=>`<article class="card"><div class="label">${c[0]}</div><div class="value">${c[1]}</div><div class="sub">${c[2]}</div></article>`).join("");document.getElementById("settings").innerHTML=[["Drone",d.drone_label],["GSD",fmt(m.gsd_cm,2)+" cm/px"],["Transect direction",fmt(m.transect_angle_deg,0)+"° from north"],["Mission start",m.start_corner],["Route order",m.reverse_order?"Reversed":"Standard"],["Transects",m.transect_count],["1.4 km trimming",trimStatus],["Capture optimisation",m.optimisation_label],["Expected photos",m.photo_count]].map(x=>`<div class="kv"><span>${x[0]}</span><span>${x[1]}</span></div>`).join("");document.getElementById("notes").innerHTML=`<p>The route and photo centres are generated from the selected AOI and current planner settings. The first image is triggered as each transect begins; interval capture is disabled on connector legs.</p><p>${trimStatus}</p><p>Flight time is route distance divided by planned speed and excludes take-off, landing, turns, acceleration, wind and operational delays. Battery count uses ${m.battery_minutes} usable minutes per battery.</p><p>Review the exported route, home point, altitude reference, obstacles, lighting, shutter speed, wind and regulatory requirements before flight.</p>`;
const R=6378137,all=[...d.aoi,...d.transects.flat(),...d.photo_points.map(p=>p.center)],lon0=all.reduce((s,p)=>s+p[0],0)/all.length,lat0=all.reduce((s,p)=>s+p[1],0)/all.length,rad=Math.PI/180,xy=p=>[(p[0]-lon0)*rad*R*Math.cos(lat0*rad),(p[1]-lat0)*rad*R],aoi=d.aoi.map(xy),lines=d.transects.map(t=>t.map(xy)),photos=d.photo_points.map(p=>({...p,xy:xy(p.center)})),points=[...aoi,...lines.flat()],xs=points.map(p=>p[0]),ys=points.map(p=>-p[1]),minX=Math.min(...xs),maxX=Math.max(...xs),minY=Math.min(...ys),maxY=Math.max(...ys),pad=Math.max(maxX-minX,maxY-minY)*.05,home={x:minX-pad,y:minY-pad,w:maxX-minX+2*pad,h:maxY-minY+2*pad};let view={...home};const svg=document.getElementById("map"),ns="http://www.w3.org/2000/svg",setView=()=>svg.setAttribute("viewBox",`${view.x} ${view.y} ${view.w} ${view.h}`);setView();const group=id=>{const g=document.createElementNS(ns,"g");g.id=id;svg.appendChild(g);return g},aoiG=group("aoi"),routeG=group("route"),fpG=group("footprints"),photoG=group("photos");const poly=(g,pts,cls)=>{const e=document.createElementNS(ns,"polygon");e.setAttribute("points",pts.map(p=>`${p[0]},${-p[1]}`).join(" "));e.setAttribute("class",cls);g.appendChild(e)};poly(aoiG,aoi,"aoi");const routePts=[];lines.forEach((line,i)=>{routePts.push(...line);const e=document.createElementNS(ns,"line");e.setAttribute("x1",line[0][0]);e.setAttribute("y1",-line[0][1]);e.setAttribute("x2",line.at(-1)[0]);e.setAttribute("y2",-line.at(-1)[1]);e.setAttribute("class","transect");routeG.appendChild(e);const t=document.createElementNS(ns,"text");t.setAttribute("x",line.at(-1)[0]);t.setAttribute("y",-line.at(-1)[1]);t.setAttribute("class","end-label");t.textContent=String(i+1);routeG.appendChild(t)});const route=document.createElementNS(ns,"polyline");route.setAttribute("points",routePts.map(p=>`${p[0]},${-p[1]}`).join(" "));route.setAttribute("class","route");routeG.insertBefore(route,routeG.firstChild);const s=document.createElementNS(ns,"circle");s.setAttribute("cx",lines[0][0][0]);s.setAttribute("cy",-lines[0][0][1]);s.setAttribute("r",Math.max(home.w,home.h)*.007);s.setAttribute("class","start");routeG.appendChild(s);photos.forEach(p=>{const th=p.bearing*rad,u=[Math.sin(th),Math.cos(th)],v=[Math.cos(th),-Math.sin(th)],hw=m.footprint_width_m/2,hh=m.footprint_length_m/2,c=p.xy,corners=[[-hh,-hw],[hh,-hw],[hh,hw],[-hh,hw]].map(q=>[c[0]+q[0]*u[0]+q[1]*v[0],c[1]+q[0]*u[1]+q[1]*v[1]]);poly(fpG,corners,"footprint");const e=document.createElementNS(ns,"circle");e.setAttribute("cx",c[0]);e.setAttribute("cy",-c[1]);e.setAttribute("r",Math.max(home.w,home.h)*.0018);e.setAttribute("class","photo");photoG.appendChild(e)});fpG.style.display="none";document.getElementById("toggle-route").onchange=e=>routeG.style.display=e.target.checked?"":"none";document.getElementById("toggle-photos").onchange=e=>photoG.style.display=e.target.checked?"":"none";document.getElementById("toggle-footprints").onchange=e=>fpG.style.display=e.target.checked?"":"none";document.getElementById("reset-view").onclick=()=>{view={...home};setView()};svg.addEventListener("wheel",e=>{e.preventDefault();const r=svg.getBoundingClientRect(),mx=view.x+(e.clientX-r.left)/r.width*view.w,my=view.y+(e.clientY-r.top)/r.height*view.h,f=e.deltaY>0?1.18:.84;view.x=mx-(mx-view.x)*f;view.y=my-(my-view.y)*f;view.w*=f;view.h*=f;setView()},{passive:false});let drag=null;svg.addEventListener("pointerdown",e=>{drag={x:e.clientX,y:e.clientY,v:{...view}};svg.setPointerCapture(e.pointerId)});svg.addEventListener("pointermove",e=>{if(!drag)return;const r=svg.getBoundingClientRect();view.x=drag.v.x-(e.clientX-drag.x)/r.width*drag.v.w;view.y=drag.v.y-(e.clientY-drag.y)/r.height*drag.v.h;setView()});svg.addEventListener("pointerup",()=>drag=null);
</script></body></html>'
  writeLines(sub("__PAYLOAD__", payload, template, fixed = TRUE), file,
             useBytes = TRUE)
  invisible(file)
}

# Read a numeric value from a DJI namespaced XML tag without depending on a
# particular WPML namespace prefix or schema version.
xml_numeric_values <- function(xml, tag_name) {
  pattern <- paste0(
    "<(?:(?:[[:alnum:]_.-]+):)?", tag_name,
    "(?:\\s[^>]*)?>\\s*([-+0-9.eE]+)"
  )
  matches <- regmatches(xml, gregexpr(pattern, xml, perl = TRUE))[[1]]
  if (length(matches) == 0 || identical(matches, character(0))) return(numeric(0))
  values <- sub(".*>\\s*", "", matches, perl = TRUE)
  values <- suppressWarnings(as.numeric(values))
  values[is.finite(values)]
}

extract_kml_coordinate_pairs <- function(xml) {
  pattern <- paste0(
    "(?s)<(?:(?:[[:alnum:]_.-]+):)?coordinates(?:\\s[^>]*)?>",
    "(.*?)</(?:(?:[[:alnum:]_.-]+):)?coordinates>"
  )
  blocks <- regmatches(xml, gregexpr(pattern, xml, perl = TRUE))[[1]]
  if (length(blocks) == 0 || identical(blocks, character(0))) {
    return(matrix(numeric(0), ncol = 2))
  }
  rows <- list()
  for (block in blocks) {
    content <- sub("^[^>]*>", "", block)
    content <- sub("</[^>]+>\\s*$", "", content, perl = TRUE)
    tokens <- strsplit(trimws(content), "[[:space:]]+", perl = TRUE)[[1]]
    for (token in tokens[nzchar(tokens)]) {
      values <- suppressWarnings(as.numeric(strsplit(token, ",", fixed = TRUE)[[1]][1:2]))
      if (length(values) == 2 && all(is.finite(values)) &&
          abs(values[1]) <= 180 && abs(values[2]) <= 90) {
        rows[[length(rows) + 1]] <- values
      }
    }
  }
  if (length(rows) == 0) return(matrix(numeric(0), ncol = 2))
  do.call(rbind, rows)
}

largest_polygon_from_kml <- function(kml_path) {
  layer_names <- tryCatch(sf::st_layers(kml_path)$name, error = function(error) character(0))
  if (length(layer_names) == 0) layer_names <- NA_character_
  candidates <- list()
  for (layer_name in layer_names) {
    features <- tryCatch(
      if (is.na(layer_name)) {
        sf::st_read(kml_path, quiet = TRUE)
      } else {
        sf::st_read(kml_path, layer = layer_name, quiet = TRUE)
      },
      error = function(error) NULL
    )
    if (is.null(features) || nrow(features) == 0) next
    polygon_rows <- sf::st_geometry_type(features) %in% c("POLYGON", "MULTIPOLYGON")
    if (!any(polygon_rows)) next
    polygons <- features[polygon_rows, ]
    if (is.na(sf::st_crs(polygons))) sf::st_crs(polygons) <- 4326
    polygons <- tryCatch(sf::st_transform(polygons, 4326), error = function(error) NULL)
    if (is.null(polygons)) next
    parts <- suppressWarnings(sf::st_cast(sf::st_geometry(polygons), "POLYGON"))
    candidates <- c(candidates, lapply(seq_along(parts), function(index) parts[[index]]))
  }
  if (length(candidates) == 0) return(NULL)
  areas <- vapply(candidates, function(polygon) {
    outer <- as.matrix(polygon[[1]])[, 1:2, drop = FALSE]
    if (nrow(outer) < 4) return(0)
    abs(geosphere::areaPolygon(outer))
  }, numeric(1))
  selected <- candidates[[which.max(areas)]]
  if (length(selected) > 1L) {
    stop(
      "KML polygons with interior holes are not supported. Remove the holes or import an exterior-only AOI."
    )
  }
  coords <- as.matrix(selected[[1]])[, 1:2, drop = FALSE]
  if (nrow(coords) > 1 && all(abs(coords[1, ] - coords[nrow(coords), ]) < 1e-10)) {
    coords <- coords[-nrow(coords), , drop = FALSE]
  }
  if (nrow(coords) < 3 || any(!is.finite(coords))) return(NULL)
  data.frame(lng = coords[, 1], lat = coords[, 2])
}

reconstruct_aoi_from_waypoints <- function(coords, fallback_width_m = 10) {
  coords <- as.matrix(coords[, 1:2, drop = FALSE])
  valid <- is.finite(coords[, 1]) & is.finite(coords[, 2]) &
    abs(coords[, 1]) <= 180 & abs(coords[, 2]) <= 90
  coords <- coords[valid, , drop = FALSE]
  if (nrow(coords) > 1) {
    duplicate_step <- c(FALSE, rowSums(abs(coords[-1, , drop = FALSE] -
      coords[-nrow(coords), , drop = FALSE])) < 1e-12)
    coords <- coords[!duplicate_step, , drop = FALSE]
  }
  if (nrow(coords) < 2) stop("The KMZ does not contain enough route points to reconstruct an AOI.")
  if (!is.finite(fallback_width_m) || fallback_width_m <= 0) fallback_width_m <- 10

  centre <- colMeans(coords)
  utm_zone <- floor((centre[1] + 180) / 6) + 1
  utm_crs <- as.integer(paste0(if (centre[2] >= 0) "326" else "327", sprintf("%02d", utm_zone)))
  points_sf <- sf::st_as_sf(
    data.frame(lng = coords[, 1], lat = coords[, 2]),
    coords = c("lng", "lat"), crs = 4326
  )
  points_utm <- sf::st_coordinates(sf::st_transform(points_sf, utm_crs))[, 1:2, drop = FALSE]

  pair_starts <- seq(1, nrow(points_utm) - 1, by = 2)
  pair_vectors <- points_utm[pair_starts + 1, , drop = FALSE] -
    points_utm[pair_starts, , drop = FALSE]
  pair_lengths <- sqrt(rowSums(pair_vectors^2))
  useful <- is.finite(pair_lengths) & pair_lengths > 1
  if (any(useful)) {
    theta <- atan2(pair_vectors[useful, 2], pair_vectors[useful, 1])
    weights <- pair_lengths[useful]
    axis_theta <- 0.5 * atan2(
      sum(weights * sin(2 * theta)), sum(weights * cos(2 * theta))
    )
  } else {
    axes <- eigen(stats::cov(points_utm))$vectors
    axis_theta <- atan2(axes[2, 1], axes[1, 1])
  }
  along <- c(cos(axis_theta), sin(axis_theta))
  cross <- c(-along[2], along[1])
  transect_midpoints <- (points_utm[pair_starts, , drop = FALSE] +
    points_utm[pair_starts + 1, , drop = FALSE]) / 2
  cross_positions <- sort(unique(round(as.numeric(transect_midpoints %*% cross), 2)))
  cross_differences <- diff(cross_positions)
  cross_differences <- cross_differences[is.finite(cross_differences) & cross_differences > 1]
  line_spacing_m <- if (length(cross_differences) > 0) stats::median(cross_differences) else 0

  # Route endpoints already describe the along-track edges. Expanding only in
  # the cross-track direction estimates the missing half-line margin without
  # extending the plan beyond its known start/end limits.
  cross_margin_m <- if (line_spacing_m > 0) line_spacing_m / 2 else fallback_width_m / 2
  expansion <- cross * cross_margin_m
  expanded <- rbind(
    sweep(points_utm, 2, expansion, "+"),
    sweep(points_utm, 2, expansion, "-")
  )
  expanded_sf <- sf::st_as_sf(
    data.frame(x = expanded[, 1], y = expanded[, 2]),
    coords = c("x", "y"), crs = utm_crs
  )
  hull <- sf::st_convex_hull(sf::st_union(sf::st_geometry(expanded_sf)))
  hull_ll <- sf::st_transform(sf::st_sfc(hull, crs = utm_crs), 4326)[[1]]
  hull_coords <- as.matrix(hull_ll[[1]])[, 1:2, drop = FALSE]
  if (nrow(hull_coords) > 1) hull_coords <- hull_coords[-nrow(hull_coords), , drop = FALSE]
  if (nrow(hull_coords) < 3) stop("The route points could not form an editable survey boundary.")

  transect_angle <- (atan2(along[1], along[2]) * 180 / pi) %% 180
  list(
    polygon = data.frame(lng = hull_coords[, 1], lat = hull_coords[, 2]),
    transect_angle = transect_angle,
    line_spacing_m = line_spacing_m
  )
}

read_survey_plan_upload <- function(upload_path, original_name) {
  extension <- tolower(tools::file_ext(original_name))
  if (!extension %in% c("kml", "kmz")) stop("Please choose a KML or KMZ file.")
  if (extension == "kml") {
    polygon <- largest_polygon_from_kml(upload_path)
    if (is.null(polygon)) stop("No polygon was found in the KML file.")
    return(list(polygon = polygon, approximate = FALSE, settings = list()))
  }

  entries <- zip::zip_list(upload_path)
  plan_entries <- entries[grepl("\\.(kml|wpml)$", entries$filename, ignore.case = TRUE), ]
  if (nrow(plan_entries) == 0) stop("The KMZ contains no KML or WPML survey-plan files.")
  unsafe <- grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", plan_entries$filename)
  if (any(unsafe)) stop("The KMZ contains an unsafe internal file path.")
  if (nrow(plan_entries) > 30 ||
      any(plan_entries$uncompressed_size > 50 * 1024^2) ||
      sum(plan_entries$uncompressed_size) > 100 * 1024^2) {
    stop("The KMZ is too large or contains too many plan files.")
  }

  preferred <- order(
    !grepl("aoi|area|boundary|survey|doc\\.kml$", plan_entries$filename, ignore.case = TRUE),
    !grepl("\\.kml$", plan_entries$filename, ignore.case = TRUE),
    plan_entries$filename
  )
  plan_entries <- plan_entries[preferred, , drop = FALSE]
  temp_dir <- tempfile("kmz_plan_import_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  xml_documents <- list()
  exact_polygon <- NULL
  for (index in seq_len(nrow(plan_entries))) {
    entry_name <- plan_entries$filename[index]
    connection <- unz(upload_path, entry_name, open = "rb")
    raw_data <- readBin(connection, what = "raw", n = plan_entries$uncompressed_size[index])
    close(connection)
    xml <- rawToChar(raw_data)
    xml_documents[[entry_name]] <- xml
    local_kml <- file.path(temp_dir, paste0("plan_", index, ".kml"))
    output_connection <- base::file(local_kml, open = "wb")
    writeBin(raw_data, output_connection)
    close(output_connection)
    polygon <- largest_polygon_from_kml(local_kml)
    if (!is.null(polygon) && is.null(exact_polygon)) exact_polygon <- polygon
  }

  combined_xml <- paste(unlist(xml_documents, use.names = FALSE), collapse = "\n")
  settings <- list(
    altitude = head(xml_numeric_values(combined_xml, "executeHeight"), 1),
    speed = head(xml_numeric_values(combined_xml, "autoFlightSpeed"), 1),
    interval = head(xml_numeric_values(combined_xml, "actionTriggerParam"), 1),
    drone_enum = head(xml_numeric_values(combined_xml, "droneEnumValue"), 1),
    drone_sub_enum = head(xml_numeric_values(combined_xml, "droneSubEnumValue"), 1),
    payload_enum = head(xml_numeric_values(combined_xml, "payloadEnumValue"), 1)
  )
  settings <- lapply(settings, function(value) if (length(value) == 0) NULL else value)

  route_names <- names(xml_documents)[grepl("waylines\\.wpml$", names(xml_documents), ignore.case = TRUE)]
  # A polygon-only KMZ is an AOI import, not a waypoint route. Only infer
  # direction/spacing from generic KML coordinates when no exact AOI exists.
  reconstruction_names <- if (length(route_names) > 0) {
    route_names
  } else if (is.null(exact_polygon)) {
    names(xml_documents)
  } else {
    character(0)
  }
  reconstruction <- NULL
  if (length(reconstruction_names) > 0) {
    route_coords <- do.call(rbind, lapply(
      xml_documents[reconstruction_names], extract_kml_coordinate_pairs
    ))
    payload_value <- settings$payload_enum
    model_match <- if (!is.null(payload_value)) {
      Filter(function(model) identical(as.numeric(model$payload_enum), as.numeric(payload_value)), dji_drone_models)
    } else list()
    fallback_width_m <- if (length(model_match) > 0 && !is.null(settings$altitude)) {
      model_match[[1]]$footprint_width_ratio * settings$altitude
    } else 10
    reconstruction <- tryCatch(
      reconstruct_aoi_from_waypoints(route_coords, fallback_width_m),
      error = function(error) NULL
    )
  }
  if (!is.null(reconstruction)) {
    settings$transect_angle <- reconstruction$transect_angle
    settings$line_spacing_m <- reconstruction$line_spacing_m
  }
  if (!is.null(exact_polygon)) {
    return(list(polygon = exact_polygon, approximate = FALSE, settings = settings))
  }
  if (is.null(reconstruction)) {
    stop("No polygon was found and the waypoint route could not form an editable AOI.")
  }
  list(
    polygon = reconstruction$polygon,
    approximate = TRUE,
    settings = settings
  )
}

# ==============================================================================
# Server Logic
# ==============================================================================
server <- function(input, output, session) {
  # Mission height is always interpreted relative to the home/takeoff point,
  # matching the exported WPML height mode for every supported aircraft.
  assumed_battery_minutes <- 25

  selected_drone <- reactive({
    model_key <- if (is.null(input$drone_model)) "m3e" else input$drone_model
    model <- dji_drone_models[[model_key]]
    if (is.null(model)) model <- dji_drone_models$m3e
    model
  })

  camera_params <- reactive(selected_drone())

  # Convert between height above home and GSD using the selected wide camera.
  height_to_gsd_cm <- function(altitude_m) {
    cam <- selected_drone()
    cam$footprint_width_ratio * altitude_m / cam$image_width * 100
  }

  gsd_cm_to_height <- function(gsd_cm) {
    cam <- selected_drone()
    gsd_cm * cam$image_width / (cam$footprint_width_ratio * 100)
  }

  is_valid_number <- function(value) {
    length(value) == 1L && is.numeric(value) && !is.na(value) && is.finite(value)
  }

  mission_altitude <- reactive({
    planning_basis <- if (is.null(input$planning_basis)) "height" else input$planning_basis
    if (identical(planning_basis, "gsd")) {
      req(is_valid_number(input$target_gsd))
      altitude_m <- gsd_cm_to_height(input$target_gsd)
    } else {
      req(is_valid_number(input$altitude))
      altitude_m <- input$altitude
    }
    max(10, min(120, altitude_m))
  })

  mission_gsd_cm <- reactive(height_to_gsd_cm(mission_altitude()))

  # Always use maximum feasible speed with a calculated fractional interval.
  capture_settings <- reactive({
    req(input$front_overlap)
    optimize_capture_settings(
      selected_drone(),
      mission_altitude(),
      input$front_overlap
    )
  })

  mission_speed <- reactive({
    capture_settings()$speed
  })

  output$camera_model_note <- renderUI({
    cam <- selected_drone()
    tags$p(
      class = "help-block",
      style = "margin-top:-8px;",
      paste0(cam$camera_note, "; DJI waypoint maximum: ", cam$max_speed, " m/s.")
    )
  })
  
  # --- Enable/disable export buttons based on polygon existence ---
  observe({
    poly_df <- drawn_polygon()
    
    if (is.null(poly_df) || nrow(poly_df) < 3) {
      # Disable all export buttons
      shinyjs::disable("download_kml")
      shinyjs::disable("download_kmz")
      shinyjs::disable("download_litchi_bundle")
      shinyjs::disable("download_plan_summary")
      shinyjs::disable("resize_polygon_area")
    } else {
      # Enable all export buttons
      shinyjs::enable("download_kml")
      shinyjs::enable("download_kmz")
      shinyjs::enable("download_litchi_bundle")
      shinyjs::enable("download_plan_summary")
      shinyjs::enable("resize_polygon_area")
    }
  })
  
  # --- Enforce altitude limits (10m to 120m) ---
  observeEvent(input$altitude, {
    altitude <- input$altitude
    # Numeric inputs briefly report NA while the user replaces their value.
    # Ignore that transient state instead of resetting the field or crashing.
    if (!is_valid_number(altitude)) return()
    bounded_altitude <- max(10, min(120, altitude))
    if (!isTRUE(all.equal(altitude, bounded_altitude))) {
      updateNumericInput(session, "altitude", value = bounded_altitude)
    }
  }, ignoreNULL = TRUE)

  # Keep the GSD control range aligned with the selected camera and the shared
  # 10-120 m planning-height envelope.
  observeEvent(input$drone_model, {
    min_gsd <- height_to_gsd_cm(10)
    max_gsd <- height_to_gsd_cm(120)
    current_gsd <- if (is_valid_number(input$target_gsd)) input$target_gsd else min_gsd
    updateNumericInput(
      session,
      "target_gsd",
      min = floor(min_gsd * 100) / 100,
      max = ceiling(max_gsd * 100) / 100,
      value = round(max(min_gsd, min(max_gsd, current_gsd)), 2)
    )
  }, ignoreInit = FALSE)

  # Keep GSD-derived missions within the same 10-120 m height envelope.
  observeEvent(input$target_gsd, {
    target_gsd <- input$target_gsd
    if (!is_valid_number(target_gsd)) return()
    min_gsd <- height_to_gsd_cm(10)
    max_gsd <- height_to_gsd_cm(120)
    bounded_gsd <- max(min_gsd, min(max_gsd, target_gsd))
    if (!isTRUE(all.equal(target_gsd, bounded_gsd))) {
      updateNumericInput(session, "target_gsd", value = round(bounded_gsd, 2))
    }
  }, ignoreNULL = TRUE)

  # --- Calculate required interval between photos (seconds) ---
  required_photo_interval <- reactive({
    capture_settings()$interval
  })

  output$height_gsd_conversion_ui <- renderUI({
    if (identical(input$planning_basis, "gsd")) {
      tags$div(
        style = "margin-bottom:6px; color:#555;",
        sprintf("Equivalent flight height above home: %.1f m", mission_altitude())
      )
    } else {
      tags$div(
        style = "margin-bottom:6px; color:#555;",
        sprintf("Estimated GSD: %.2f cm/pixel", mission_gsd_cm())
      )
    }
  })

  output$auto_speed_ui <- renderUI({
    capture <- capture_settings()
    tags$div(
      style = "margin-bottom:10px; color:#0072B2; font-weight:bold;",
      sprintf(
        "Maximum-efficiency capture: %.1f m/s at %.2f s intervals (%.1f%% forward overlap)",
        capture$speed,
        capture$interval,
        capture$achieved_overlap
      )
    )
  })

  # --- KML/KMZ Import Support ---
  # sf reads KML polygons; DJI WPML route coordinates are handled separately.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Please install the 'sf' package for KML/KMZ import: install.packages('sf')")
  }


  # --- Reactive Value to Store Drawn Polygon ---
  drawn_polygon <- reactiveVal(NULL)

  # Keep the AOI appearance identical whether it is drawn, imported, resized,
  # vertex-edited, or moved. These options are also used by Leaflet.Draw so a
  # saved edit returns to the same blue, 25%-opacity style.
  aoi_color <- "#3388ff"
  aoi_fill_opacity <- 0.25
  aoi_polygon_options <- function() {
    leaflet.extras::drawPolygonOptions(
      showArea = TRUE,
      repeatMode = FALSE,
      shapeOptions = leaflet.extras::drawShapeOptions(
        color = aoi_color,
        weight = 2,
        opacity = 1,
        fill = TRUE,
        fillColor = aoi_color,
        fillOpacity = aoi_fill_opacity
      )
    )
  }
  aoi_edit_options <- function() {
    leaflet.extras::editToolbarOptions(
      selectedPathOptions = leaflet.extras::selectedPathOptions(
        weight = 2,
        color = aoi_color,
        fill = TRUE,
        fillColor = aoi_color,
        fillOpacity = aoi_fill_opacity,
        maintainColor = TRUE
      )
    )
  }
  aoi_replace_state <- new.env(parent = emptyenv())
  aoi_replace_state$generation <- 0L

  # --- Observer for KML/KMZ AOI and Survey Plan Import ---
  observeEvent(input$kml_file, {
    req(input$kml_file)
    imported <- tryCatch(
      read_survey_plan_upload(
        input$kml_file$datapath,
        input$kml_file$name
      ),
      error = function(error) {
        showNotification(
          paste("Could not import survey plan:", conditionMessage(error)),
          type = "error",
          duration = 8
        )
        NULL
      }
    )
    if (is.null(imported)) return()

    coords_df <- imported$polygon[, c("lng", "lat"), drop = FALSE]
    if (nrow(coords_df) < 3 || any(!is.finite(as.matrix(coords_df)))) {
      showNotification(
        "The imported plan did not produce a valid editable survey boundary.",
        type = "error",
        duration = 7
      )
      return()
    }
    geometry_ok <- tryCatch(
      {
        validate_aoi_polygon(coords_df)
        TRUE
      },
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
        FALSE
      }
    )
    if (!geometry_ok) return()
    drawn_polygon(coords_df)
    replace_editable_aoi_on_map(coords_df)

    leafletProxy("map", session) %>%
      fitBounds(
        lng1 = min(coords_df$lng), lat1 = min(coords_df$lat),
        lng2 = max(coords_df$lng), lat2 = max(coords_df$lat)
      )

    settings <- imported$settings
    valid_scalar <- function(value) {
      length(value) == 1L && is.numeric(value) && is.finite(value)
    }
    model_keys <- names(dji_drone_models)
    model_scores <- vapply(model_keys, function(key) {
      model <- dji_drone_models[[key]]
      score <- 0
      if (valid_scalar(settings$payload_enum) &&
          settings$payload_enum == model$payload_enum) score <- score + 8
      if (valid_scalar(settings$drone_enum) &&
          settings$drone_enum == model$drone_enum) score <- score + 2
      if (valid_scalar(settings$drone_sub_enum) &&
          settings$drone_sub_enum == model$drone_sub_enum) score <- score + 1
      score
    }, numeric(1))
    model_key <- if (max(model_scores) > 0) {
      model_keys[[which.max(model_scores)]]
    } else if (is.null(input$drone_model)) {
      "m3e"
    } else {
      input$drone_model
    }
    model <- dji_drone_models[[model_key]]
    if (max(model_scores) > 0) {
      updateSelectInput(session, "drone_model", selected = model_key)
    }

    altitude <- if (valid_scalar(settings$altitude)) {
      max(10, min(120, settings$altitude))
    } else {
      NULL
    }
    if (!is.null(altitude)) {
      updateRadioButtons(session, "planning_basis", selected = "height")
      updateNumericInput(session, "altitude", value = round(altitude, 2))
    }

    if (valid_scalar(settings$transect_angle)) {
      updateSliderInput(
        session, "transect_angle",
        value = round(settings$transect_angle) %% 180
      )
    }

    if (!is.null(altitude) &&
        valid_scalar(settings$speed) && valid_scalar(settings$interval)) {
      footprint_length <- model$footprint_height_ratio * altitude
      imported_overlap <- 100 * (
        1 - settings$speed * settings$interval / footprint_length
      )
      imported_overlap <- max(0, min(95, imported_overlap))
      updateSliderInput(
        session, "front_overlap",
        value = round(imported_overlap, 1)
      )
    }

    if (!is.null(altitude) && valid_scalar(settings$line_spacing_m)) {
      imported_gap <- max(
        0,
        settings$line_spacing_m - model$footprint_width_ratio * altitude
      )
      updateNumericInput(
        session, "transect_gap_m",
        value = round(imported_gap, 1)
      )
    }

    if (isTRUE(imported$approximate)) {
      showNotification(
        paste(
          "KMZ route imported. This plan did not contain an AOI polygon, so an",
          "editable boundary was reconstructed from its waypoints. Review the",
          "boundary before exporting a modified mission."
        ),
        type = "warning",
        duration = 12
      )
    } else {
      showNotification(
        paste0(
          toupper(tools::file_ext(input$kml_file$name)),
          " survey boundary imported successfully."
        ),
        type = "message",
        duration = 6
      )
    }
  })
  polygon_from_draw_geojson <- function(geojson) {
    if (is.null(geojson)) return(NULL)
    features <- if (identical(geojson$type, "FeatureCollection")) {
      geojson$features
    } else {
      list(geojson)
    }
    if (length(features) == 0) return(NULL)

    for (feature in features) {
      geometry <- feature$geometry
      if (is.null(geometry) || !identical(geometry$type, "Polygon")) next
      coordinates <- geometry$coordinates[[1]]
      if (length(geometry$coordinates) > 1L) {
        stop("Polygons with interior holes are not supported by the editable AOI.")
      }
      coords_matrix <- do.call(rbind, coordinates)
      coords_df <- data.frame(
        lng = as.numeric(coords_matrix[, 1]),
        lat = as.numeric(coords_matrix[, 2])
      )
      if (nrow(coords_df) > 1 &&
          all(abs(coords_df[1, ] - coords_df[nrow(coords_df), ]) < 1e-10)) {
        coords_df <- coords_df[-nrow(coords_df), , drop = FALSE]
      }
      if (nrow(coords_df) >= 3) return(coords_df[, c("lng", "lat")])
    }
    NULL
  }

  accept_drawn_polygon <- function(coords_df) {
    tryCatch(
      {
        if (is.null(coords_df)) stop("The drawn feature is not a usable polygon.")
        validate_aoi_polygon(coords_df)
        drawn_polygon(coords_df)
        TRUE
      },
      error = function(error) {
        showNotification(
          paste("Survey boundary was not accepted:", conditionMessage(error)),
          type = "error",
          duration = 8
        )
        replace_editable_aoi_on_map(drawn_polygon())
        FALSE
      }
    )
  }

  # New, vertex-edited, and whole-polygon-dragged AOIs all update the same
  # server-side geometry used for mission calculations and exports.
  observeEvent(input$map_draw_new_feature, {
    coords_df <- tryCatch(
      polygon_from_draw_geojson(input$map_draw_new_feature),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
        NULL
      }
    )
    accept_drawn_polygon(coords_df)
  })

  observeEvent(input$map_draw_edited_features, {
    coords_df <- tryCatch(
      polygon_from_draw_geojson(input$map_draw_edited_features),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
        NULL
      }
    )
    accept_drawn_polygon(coords_df)
  })

  observeEvent(input$map_draw_deleted_features, {
    drawn_polygon(NULL)
  })

  # Replace the client-side Draw feature group only for server-generated AOIs
  # (e.g. buffering or KML/KMZ import). User edits and moves already update the
  # existing Leaflet.Draw polygon and must not rebuild it during Save.
  replace_editable_aoi_on_map <- function(poly_df) {
    aoi_replace_state$generation <- aoi_replace_state$generation + 1L
    replace_generation <- aoi_replace_state$generation
    map_proxy <- leafletProxy("map", session)
    map_proxy %>%
      # Remove server-registered layers before Leaflet.Draw clears its feature
      # group. Reversing this order leaves stale Leaflet layer-manager IDs,
      # which can abort the replacement and make the blue AOI disappear.
      leaflet::clearGroup("drawn_poly") %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)

    # Send the replacement in the next event-loop tick. This guarantees the
    # client has removed the old Draw feature before the new editable AOI is
    # added, avoiding both a vanished polygon and two stacked 25% fills.
    later::later(function() {
      current_generation <- aoi_replace_state$generation
      if (!identical(replace_generation, current_generation)) return()
      # Create a one-shot observer so the proxy work itself runs with a live
      # reactive context. Timer callbacks cannot safely call leafletProxy.
      shiny::observeEvent(
        TRUE,
        {
          if (!identical(replace_generation, aoi_replace_state$generation)) return()
          replacement_proxy <- leafletProxy("map", session)
          if (!is.null(poly_df) && nrow(poly_df) >= 3) {
            replacement_proxy <- replacement_proxy %>% addPolygons(
              lng = as.numeric(poly_df$lng),
              lat = as.numeric(poly_df$lat),
              color = aoi_color,
              fillColor = aoi_color,
              fillOpacity = aoi_fill_opacity,
              weight = 2,
              group = "drawn_poly",
              layerId = "active-aoi"
            )
          }
          replacement_proxy %>% leaflet.extras::addDrawToolbar(
            targetGroup = "drawn_poly",
            polylineOptions = FALSE,
            polygonOptions = aoi_polygon_options(),
            circleOptions = FALSE,
            rectangleOptions = FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE,
            editOptions = aoi_edit_options(),
            singleFeature = TRUE,
            drag = TRUE
          )
        },
        once = TRUE,
        ignoreInit = FALSE,
        domain = session
      )
    }, delay = 0.3)
  }

  # Advisory restriction import. Warnings derive from drawn_polygon(), so they
  # update after draws, edits, drags, imports, resizes, and rotations. Hiding
  # the overlay never suppresses the warning.
  restriction_zones <- restriction_tools_server(input, output, session, drawn_polygon)

  # --- Observer for Clear Polygon Button ---
  observeEvent(input$clear_polygon, {
    drawn_polygon(NULL)
    replace_editable_aoi_on_map(NULL)
    
    # Clear all map elements
    map_proxy <- leafletProxy("map")
    
    # Route layers are cleared here; the dedicated AOI observer above replaces
    # the editable Draw feature group.
    map_proxy %>% 
      leaflet::clearGroup("transects") %>%
      leaflet::clearGroup("photopoints") %>%
      leaflet::clearGroup("footprints")
  })
  
  # --- Calculate Optimal Transect Direction ---
  observeEvent(input$calc_optimal_direction, {
    poly_df <- drawn_polygon()
    
    if (is.null(poly_df)) {
      showNotification("Please draw a polygon first!", type = "warning")
      return()
    }
    
    optimal_angle <- calculate_optimal_direction(poly_df)
    updateSliderInput(session, "transect_angle", value = optimal_angle)
    showNotification(
      paste0("Optimal transect direction: ", optimal_angle, "° (minimizes number of transects)"),
      type = "message",
      duration = 5
    )
  })

  observeEvent(input$align_polygon_edges, {
    poly_df <- drawn_polygon()
    if (is.null(poly_df) || nrow(poly_df) < 3) {
      showNotification("Please draw a polygon first!", type = "warning")
      return()
    }

    alignment <- tryCatch(
      calculate_polygon_alignment(poly_df),
      error = function(error) {
        showNotification(
          paste("Could not align to this polygon:", conditionMessage(error)),
          type = "error",
          duration = 6
        )
        NULL
      }
    )
    if (is.null(alignment)) return()

    selected_axis <- if (identical(input$polygon_alignment_axis, "short")) "short" else "long"
    aligned_angle <- round(unname(alignment[[selected_axis]])) %% 180
    updateSliderInput(session, "transect_angle", value = aligned_angle)
    edge_label <- if (selected_axis == "long") "top/bottom (long)" else "side (short)"
    showNotification(
      sprintf("Flight lines aligned to %s polygon edges: %d degrees", edge_label, aligned_angle),
      type = "message",
      duration = 5
    )
  })

  # --- Render the Initial Map with Draw Toolbar ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addWMSTiles(
        baseUrl = dea_wms_url,
        layers = dea_tidal_layer,
        group = "DEA Low Tide",
        options = WMSTileOptions(
          styles = "low_true",
          format = "image/png",
          transparent = FALSE,
          version = "1.1.1"
        ),
        attribution = dea_attribution
      ) %>%
      addWMSTiles(
        baseUrl = dea_wms_url,
        layers = dea_tidal_layer,
        group = "DEA High Tide",
        options = WMSTileOptions(
          styles = "high_true",
          format = "image/png",
          transparent = FALSE,
          version = "1.1.1"
        ),
        attribution = dea_attribution
      ) %>%
      addWMSTiles(
        baseUrl = dea_wms_url,
        layers = dea_intertidal_layer,
        group = "DEA Intertidal Extent (2024)",
        options = WMSTileOptions(
          styles = "intertidal_extents",
          format = "image/png",
          transparent = FALSE,
          version = "1.1.1",
          time = "2024-01-01"
        ),
        attribution = dea_attribution
      ) %>%
      setView(lng = 146.8169, lat = -19.2590, zoom = 7) %>% # Center on Townsville, showing entire QLD coast
      addLayersControl(
        baseGroups = c(
          "Satellite", "Street Map", "DEA Low Tide", "DEA High Tide",
          "DEA Intertidal Extent (2024)"
        ),
        overlayGroups = c(restriction_group),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addControl(
        html = HTML(paste0(
          "<strong>DEA Intertidal Extent (2024)</strong><br>",
          "<img src=\"", dea_intertidal_legend_url, "\" ",
          "alt=\"DEA Intertidal Extent class legend\" ",
          "style=\"display:block;max-width:280px;height:auto;margin-top:4px;\">"
        )),
        position = "bottomright",
        layerId = "dea-intertidal-legend",
        className = "info legend dea-intertidal-legend"
      ) %>%
      htmlwidgets::onRender(
        "function(el, x) {
          var map = this;
          var legend = el.querySelector('.dea-intertidal-legend');
          if (!legend) return;
          legend.style.display = 'none';
          map.on('baselayerchange', function(event) {
            legend.style.display =
              event.name === 'DEA Intertidal Extent (2024)' ? '' : 'none';
          });
        }"
      ) %>%
      leaflet::addMeasure(
        position = "topright",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        secondaryAreaUnit = "hectares"
      ) %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "drawn_poly",
        polylineOptions = FALSE,
        polygonOptions = aoi_polygon_options(),
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = aoi_edit_options(),
        singleFeature = TRUE,
        drag = TRUE
      )
  })


  # --- Helper: Generate Transects and Photo Points ---
  generate_transects_and_photopoints <- function(poly_df, altitude, transect_gap_m,
                                                  cam, transect_angle = 0,
                                                  trim_long_transects = FALSE) {
    # Returns list(transects = list of matrices), photo_points = list of matrices
    if (is.null(poly_df) || nrow(poly_df) < 3) return(list(transects = list(), photo_points = list()))
    # Defensive checks for inputs
    if (is.null(transect_angle) || length(transect_angle) == 0 || !is.numeric(transect_angle)) transect_angle <- 0
    if (is.null(altitude) || length(altitude) == 0 || !is.numeric(altitude)) return(list(transects = list(), photo_points = list()))
    if (is.null(transect_gap_m) || length(transect_gap_m) == 0 || !is.numeric(transect_gap_m)) return(list(transects = list(), photo_points = list()))
    # Calculate effective ground footprint
    footprint_width_m <- cam$footprint_width_ratio * altitude
    footprint_height_m <- cam$footprint_height_ratio * altitude
    eff_width <- footprint_width_m + max(0, transect_gap_m)
    eff_height <- capture_settings()$actual_spacing_m
    # Get bounding box in UTM
    requireNamespace("sf")
    # Ensure polygon is closed for sf (first point == last point)
    poly_matrix <- as.matrix(poly_df[, c("lng", "lat")])
    if (!all(poly_matrix[1,] == poly_matrix[nrow(poly_matrix),])) {
      poly_matrix <- rbind(poly_matrix, poly_matrix[1,])
    }
    poly_sf <- sf::st_polygon(list(poly_matrix))
    poly_sf <- sf::st_sfc(poly_sf, crs = 4326)
    # Automatically determine correct UTM zone from polygon centroid
    centroid <- sf::st_coordinates(sf::st_centroid(poly_sf))
    lon <- centroid[1, "X"]
    lat <- centroid[1, "Y"]
    # Calculate UTM zone: zone = floor((lon + 180) / 6) + 1
    utm_zone <- floor((lon + 180) / 6) + 1
    # Determine if northern or southern hemisphere
    utm_crs <- if (lat >= 0) {
      paste0("326", sprintf("%02d", utm_zone))  # Northern hemisphere (WGS84)
    } else {
      paste0("327", sprintf("%02d", utm_zone))  # Southern hemisphere (WGS84)
    }
    poly_utm <- suppressWarnings(sf::st_transform(poly_sf, as.numeric(utm_crs)))
    # Fly slightly beyond the AOI edge so complete rectangular footprints cover
    # sloped boundaries between adjacent swaths. The half-diagonal is the
    # smallest direction-independent margin for the camera footprint.
    coverage_buffer_m <- sqrt((footprint_width_m / 2)^2 + (footprint_height_m / 2)^2)
    coverage_poly_utm <- suppressWarnings(sf::st_buffer(poly_utm, dist = coverage_buffer_m))
    bbox_utm <- sf::st_bbox(poly_utm)
    # Center of bbox
    center_x <- (bbox_utm["xmin"] + bbox_utm["xmax"]) / 2
    center_y <- (bbox_utm["ymin"] + bbox_utm["ymax"]) / 2
    # Length of bbox diagonal (to ensure coverage)
    diag_len <- sqrt((bbox_utm["xmax"] - bbox_utm["xmin"])^2 + (bbox_utm["ymax"] - bbox_utm["ymin"])^2)
    candidate_line_length <- diag_len + 2 * coverage_buffer_m
    # Angle in radians (convert from degrees, 0 = North, clockwise)
    theta <- transect_angle * pi / 180
    # Transect direction vector (0° = North = positive Y in UTM)
    tx <- sin(theta)
    ty <- cos(theta)
    # Direction vector perpendicular to transect (for offsetting)
    dx <- -ty  # perpendicular is 90° clockwise
    dy <- tx
    # Cover the complete cross-track span. Ceiling prevents uncovered strips
    # along both AOI edges when the span is not an exact footprint multiple.
    polygon_coords <- sf::st_coordinates(poly_utm)
    cross_track <- polygon_coords[, "X"] * dx + polygon_coords[, "Y"] * dy
    cross_track_min <- min(cross_track)
    cross_track_max <- max(cross_track)
    cross_track_span <- cross_track_max - cross_track_min
    # KML/WPML coordinates and imported gap controls are rounded; ignore up to
    # one centimetre of reconstruction noise at exact footprint multiples.
    span_tolerance_m <- max(
      0.01,
      sqrt(.Machine$double.eps) * max(1, cross_track_span, eff_width)
    )
    n_transects <- max(
      1,
      ceiling(max(0, cross_track_span - span_tolerance_m) / eff_width)
    )
    grid_center <- (cross_track_min + cross_track_max) / 2
    center_cross_track <- center_x * dx + center_y * dy
    centered_steps <- seq_len(n_transects) - (n_transects + 1) / 2
    offsets <- grid_center - center_cross_track + centered_steps * eff_width
    transects <- list()
    photo_points <- list()
    trimmed_transect_count <- 0L
    original_transect_lengths_m <- numeric()
    for (i in seq_along(offsets)) {
      # Offset from center
      ox <- offsets[i] * dx
      oy <- offsets[i] * dy
      # Start and end points of transect in UTM
      x0 <- center_x + ox - tx * candidate_line_length/2
      y0 <- center_y + oy - ty * candidate_line_length/2
      x1 <- center_x + ox + tx * candidate_line_length/2
      y1 <- center_y + oy + ty * candidate_line_length/2
      line <- sf::st_linestring(matrix(c(x0, y0, x1, y1), ncol = 2, byrow = TRUE))
      line <- sf::st_sfc(line, crs = as.numeric(utm_crs))
      clipped <- suppressWarnings(sf::st_intersection(line, coverage_poly_utm))
      if (length(clipped) > 0 && !all(sf::st_is_empty(clipped))) {
        # A concave AOI can yield a MULTILINESTRING. Cast it into individual
        # LINESTRING parts so every valid survey segment is retained.
        line_parts <- suppressWarnings(sf::st_cast(clipped, "LINESTRING"))
        for (part_index in seq_along(line_parts)) {
          part <- line_parts[part_index]
          coords_ll <- sf::st_transform(part, 4326)
          coords_ll <- sf::st_coordinates(coords_ll)
          if (nrow(coords_ll) < 2) next
          original_length_m <- transect_length_m(coords_ll)
          original_transect_lengths_m <- c(
            original_transect_lengths_m, original_length_m
          )
          if (isTRUE(trim_long_transects) &&
              original_length_m > transect_length_limit_m) {
            coords_ll <- trim_transect_to_length(
              coords_ll, transect_length_limit_m
            )
            trimmed_transect_count <- trimmed_transect_count + 1L
          }
          transects[[length(transects) + 1]] <- coords_ll
        }
      }
    }
    photo_points <- generate_photo_points_for_transects(transects, eff_height)
    list(
      transects = transects,
      photo_points = photo_points,
      trimmed_transect_count = trimmed_transect_count,
      original_longest_transect_m = if (length(original_transect_lengths_m) > 0) {
        max(original_transect_lengths_m)
      } else 0
    )
  }

  notify_oversized_transects <- function(transects, export_label) {
    route_metrics <- measure_transect_route(transects)
    oversized_count <- sum(
      route_metrics$transect_lengths_m > transect_length_limit_m + 1e-6
    )
    if (oversized_count > 0) {
      showNotification(
        sprintf(
          paste0(
            "Warning: %d transect%s exceed%s 1.4 km (longest %.2f km). ",
            "%s will continue; assess line of sight or enable trimming."
          ),
          oversized_count,
          if (oversized_count == 1L) "" else "s",
          if (oversized_count == 1L) "s" else "",
          route_metrics$longest_transect_m / 1000,
          export_label
        ),
        type = "warning",
        duration = 10
      )
    }
    invisible(oversized_count)
  }

  coordinates_as_list <- function(coords) {
    lapply(seq_len(nrow(coords)), function(index) {
      unname(as.numeric(coords[index, 1:2]))
    })
  }

  # Build the downloadable report from the same ordered transects used by the
  # aircraft exports. Unlike the compact on-screen estimate, route distance,
  # flight time and photo count are measured directly from the generated plan.
  build_plan_summary_data <- reactive({
    poly_df <- drawn_polygon()
    req(!is.null(poly_df), nrow(poly_df) >= 3)
    req(input$front_overlap, input$transect_gap_m)

    altitude <- mission_altitude()
    cam <- camera_params()
    capture <- capture_settings()
    speed <- mission_speed()
    angle <- if (is_valid_number(input$transect_angle)) input$transect_angle else 0
    result <- generate_transects_and_photopoints(
      poly_df, altitude, input$transect_gap_m, cam, angle,
      input$trim_transect_length
    )
    validate(need(length(result$transects) > 0, "No survey transects were generated."))

    start_corner <- if (is.null(input$start_corner)) "top_left" else input$start_corner
    ordered_transects <- reorder_transects_by_start(
      result$transects, start_corner, angle
    )
    ordered_transects <- apply_reverse_transect_order(
      ordered_transects, input$reverse_transect_order
    )

    route_metrics <- measure_transect_route(ordered_transects)
    route_distance_m <- route_metrics$route_distance_m

    photo_points_by_transect <- generate_photo_points_for_transects(
      ordered_transects, capture$actual_spacing_m
    )
    photo_points <- list()
    for (centres in photo_points_by_transect) {
      bearing <- attr(centres, "bearing")
      for (index in seq_len(nrow(centres))) {
        photo_points[[length(photo_points) + 1]] <- list(
          center = unname(as.numeric(centres[index, 1:2])),
          bearing = unname(as.numeric(bearing))
        )
      }
    }

    aoi_coords <- as.matrix(poly_df[, c("lng", "lat")])
    if (!all(aoi_coords[1, ] == aoi_coords[nrow(aoi_coords), ])) {
      aoi_coords <- rbind(aoi_coords, aoi_coords[1, ])
    }
    flight_time_min <- route_distance_m / speed / 60
    start_labels <- c(
      top_left = "Top left", top_right = "Top right",
      bottom_left = "Bottom left", bottom_right = "Bottom right"
    )
    optimisation_label <- "Maximum efficiency"

    list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
      drone_label = selected_drone()$label,
      aoi = coordinates_as_list(aoi_coords),
      transects = lapply(ordered_transects, coordinates_as_list),
      photo_points = photo_points,
      metrics = list(
        area_ha = polygon_area_m2() / 10000,
        altitude_m = altitude,
        speed_mps = speed,
        optimisation_label = optimisation_label,
        photo_interval_s = capture$interval,
        forward_overlap_pct = capture$achieved_overlap,
        footprint_width_m = cam$footprint_width_ratio * altitude,
        footprint_length_m = cam$footprint_height_ratio * altitude,
        footprint_gap_m = max(0, input$transect_gap_m),
        gsd_cm = mission_gsd_cm(),
        photo_count = length(photo_points),
        transect_count = length(ordered_transects),
        longest_transect_m = route_metrics$longest_transect_m,
        longest_transect_km = route_metrics$longest_transect_m / 1000,
        original_longest_transect_m = result$original_longest_transect_m,
        original_longest_transect_km = result$original_longest_transect_m / 1000,
        trimmed_transect_count = result$trimmed_transect_count,
        oversized_transect_count = sum(
          route_metrics$transect_lengths_m > transect_length_limit_m + 1e-6
        ),
        trim_transect_length_enabled = isTRUE(input$trim_transect_length),
        transect_length_limit_m = transect_length_limit_m,
        transect_length_limit_km = transect_length_limit_m / 1000,
        survey_distance_km = route_metrics$survey_distance_m / 1000,
        connector_distance_km = route_metrics$connector_distance_m / 1000,
        route_distance_km = route_distance_m / 1000,
        flight_time_min = flight_time_min,
        batteries = max(1L, as.integer(ceiling(
          flight_time_min / assumed_battery_minutes
        ))),
        battery_minutes = assumed_battery_minutes,
        transect_angle_deg = angle,
        start_corner = unname(start_labels[[start_corner]]),
        reverse_order = isTRUE(input$reverse_transect_order)
      )
    )
  })

  litchi_native_plan <- reactive({
    poly_df <- drawn_polygon()
    req(!is.null(poly_df), nrow(poly_df) >= 3)
    req(input$front_overlap, input$transect_gap_m)

    altitude <- mission_altitude()
    speed <- mission_speed()
    cam <- camera_params()
    angle <- if (is.null(input$transect_angle)) 0 else input$transect_angle
    result <- generate_transects_and_photopoints(
      poly_df, altitude, input$transect_gap_m, cam, angle,
      input$trim_transect_length
    )
    validate(need(length(result$transects) > 0, "No survey transects were generated."))

    start_corner <- if (is.null(input$start_corner)) "top_left" else input$start_corner
    ordered_transects <- reorder_transects_by_start(
      result$transects, start_corner, angle
    )
    ordered_transects <- apply_reverse_transect_order(
      ordered_transects, input$reverse_transect_order
    )
    build_litchi_native_plan(
      ordered_transects, altitude, speed, capture_settings()$interval,
      selected_drone()
    )
  })

  # --- Reactive Observer to Update Map with Drawn Polygon, Transects, and Photo Points ---
  observeEvent(
    list(
      drawn_polygon(),
      mission_altitude(),
      input$front_overlap,
      input$transect_gap_m,
      input$show_transects,
      input$show_photopoints,
      input$show_footprints,
      input$drone_model,
      input$transect_angle,
      input$trim_transect_length,
      input$start_corner,
      input$reverse_transect_order
    ),
    {
      # Defensive checks for required inputs
      req(input$front_overlap, input$transect_gap_m)
      altitude <- mission_altitude()
      
      poly_df <- drawn_polygon()
      map_proxy <- leafletProxy("map", session)
      map_proxy %>%
        clearGroup("transects") %>%
        clearGroup("photopoints") %>%
        clearGroup("footprints")
      if (!is.null(poly_df) && nrow(poly_df) > 2) {
        # Show transects and/or photo points if requested
        if (input$show_transects || input$show_photopoints || input$show_footprints) {
          cam <- camera_params()
          # Defensive check for transect_angle
          angle <- if (is.null(input$transect_angle)) 0 else input$transect_angle
          res <- tryCatch(
            generate_transects_and_photopoints(
              poly_df, altitude, input$transect_gap_m, cam, angle,
              input$trim_transect_length
            ),
            error = function(error) {
              showNotification(
                paste("Could not generate survey route:", conditionMessage(error)),
                type = "error",
                duration = 8
              )
              NULL
            }
          )
          if (is.null(res)) return()
          
          # Reorder transects based on start corner
          start_corner <- if (is.null(input$start_corner)) "top_left" else input$start_corner
          transects_display <- reorder_transects_by_start(res$transects, start_corner, angle)
          transects_display <- apply_reverse_transect_order(
            transects_display, input$reverse_transect_order
          )
          photo_points_display <- generate_photo_points_for_transects(
            transects_display, capture_settings()$actual_spacing_m
          )
          
          # Transects
          if (input$show_transects && length(transects_display) > 0) {
            for (i in seq_along(transects_display)) {
              coords <- transects_display[[i]]
              # Draw transect line (solid orange)
              map_proxy %>% addPolylines(lng = coords[,1], lat = coords[,2], color = "orange", weight = 2, group = "transects")
              
              # Draw connection to next transect (solid orange line)
              if (i < length(transects_display)) {
                next_coords <- transects_display[[i + 1]]
                # Connect end of current transect to start of next transect
                connection <- matrix(c(
                  coords[nrow(coords), 1], coords[nrow(coords), 2],
                  next_coords[1, 1], next_coords[1, 2]
                ), ncol = 2, byrow = TRUE)
                map_proxy %>% addPolylines(
                  lng = connection[,1], 
                  lat = connection[,2], 
                  color = "orange", 
                  weight = 2, 
                  group = "transects"
                )
              }
              
              # Numbered marker at end
              map_proxy %>% addLabelOnlyMarkers(lng = coords[nrow(coords),1], lat = coords[nrow(coords),2],
                label = as.character(i), labelOptions = labelOptions(noHide = TRUE, direction = 'auto', textOnly = TRUE, style = list("color" = "black", "font-weight" = "bold", "font-size" = "14px")), group = "transects")
            }
            # Add red START marker at the first waypoint
            start_point <- transects_display[[1]][1, ]
            map_proxy %>% addCircleMarkers(
              lng = start_point[1], 
              lat = start_point[2], 
              radius = 8, 
              color = "red", 
              fillColor = "red",
              fillOpacity = 1, 
              weight = 3,
              group = "transects",
              popup = "Mission START"
            )
            # Add START label
            map_proxy %>% addLabelOnlyMarkers(
              lng = start_point[1], 
              lat = start_point[2],
              label = "START",
              labelOptions = labelOptions(
                noHide = TRUE, 
                direction = 'right', 
                textOnly = TRUE, 
                style = list(
                  "color" = "white", 
                  "font-weight" = "bold", 
                  "font-size" = "14px",
                  "text-shadow" = "2px 2px 4px rgba(0,0,0,0.8)"
                )
              ),
              group = "transects"
            )
          }
          # Photo points
          if ((input$show_photopoints || input$show_footprints) && length(photo_points_display) > 0) {
            for (pts in photo_points_display) {
              if (input$show_photopoints) {
                map_proxy %>% addCircleMarkers(lng = pts[,1], lat = pts[,2], radius = 2, color = "red", fillOpacity = 0.7, group = "photopoints")
              }
              if (input$show_footprints) {
                # Draw rectangle for each photo footprint, rotated to transect direction
                # Wide-camera footprint width is perpendicular to flight;
                # footprint height is parallel to the transect.
                footprint_width_m <- cam$footprint_width_ratio * altitude
                footprint_height_m <- cam$footprint_height_ratio * altitude
                for (j in seq_len(nrow(pts))) {
                  center <- pts[j, 1:2]
                  # Determine bearing: use next point if possible, else previous
                  if (j < nrow(pts)) {
                    bearing <- geosphere::bearing(center, pts[j+1, 1:2])
                  } else if (j > 1) {
                    bearing <- geosphere::bearing(pts[j-1, 1:2], center)
                  } else {
                    bearing <- attr(pts, "bearing")
                  }
                  # Rectangle corners: X = perpendicular (width), Y = parallel (height)
                  half_w <- footprint_width_m / 2  # Perpendicular to flight
                  half_h <- footprint_height_m / 2  # Parallel to flight
                  # Corners relative to center (meters) - swapped to match orientation
                  corners <- matrix(c(
                    -half_h, -half_w,
                    half_h, -half_w,
                    half_h, half_w,
                    -half_h, half_w,
                    -half_h, -half_w
                  ), ncol = 2, byrow = TRUE)
                  # Rotate corners by bearing
                  theta <- bearing * pi / 180
                  rot <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
                  rotated <- t(rot %*% t(corners))
                  # Convert to lat/lon using destPoint
                  # Calculate bearing and distance for each corner from center
                  corner_bearings <- (atan2(rotated[,1], rotated[,2]) * 180/pi + 90) %% 360
                  corner_distances <- sqrt(rotated[,1]^2 + rotated[,2]^2)
                  # Use correct parameter names: p (point), b (bearing), d (distance)
                  poly_coords <- geosphere::destPoint(p = center, b = corner_bearings, d = corner_distances)
                  poly_lng <- poly_coords[,1]
                  poly_lat <- poly_coords[,2]
                  map_proxy %>% addPolygons(lng = poly_lng, lat = poly_lat, color = "#00FF00", weight = 1, fillOpacity = 0.2, group = "footprints")
                }
              }
            }
          }
        }
      }
    },
    ignoreNULL = FALSE
  )


  # --- Reactive Calculations ---
  # Calculate polygon area
  polygon_area_m2 <- reactive({
    poly_df <- drawn_polygon()
    if (is.null(poly_df) || nrow(poly_df) < 3) return(0)
    coords_matrix <- as.matrix(poly_df[, c("lng", "lat")])
    # Ensure polygon is closed (first == last)
    if (!all(coords_matrix[1,] == coords_matrix[nrow(coords_matrix),])) {
      coords_matrix <- rbind(coords_matrix, coords_matrix[1,])
    }
    areaPolygon(coords_matrix)
  })

  area_unit_factor <- function(unit) {
    if (identical(unit, "acres")) 4046.8564224 else 10000
  }

  observeEvent(list(drawn_polygon(), input$target_area_unit), {
    area_m2 <- polygon_area_m2()
    if (area_m2 <= 0) return()
    factor <- area_unit_factor(input$target_area_unit)
    updateNumericInput(
      session,
      "target_area",
      value = signif(max(area_m2 / factor, 0.0001), 6)
    )
  }, ignoreInit = FALSE)

  observeEvent(input$resize_polygon_area, {
    poly_df <- drawn_polygon()
    if (is.null(poly_df) || nrow(poly_df) < 3) {
      showNotification("Please draw or import a polygon first!", type = "warning")
      return()
    }
    req(input$target_area)
    factor <- area_unit_factor(input$target_area_unit)
    target_area_m2 <- input$target_area * factor

    result <- tryCatch(
      resize_polygon_to_target_area(poly_df, target_area_m2),
      error = function(error) {
        showNotification(
          paste("Could not resize the polygon:", conditionMessage(error)),
          type = "error",
          duration = 7
        )
        NULL
      }
    )
    if (is.null(result)) return()

    drawn_polygon(result$polygon)
    replace_editable_aoi_on_map(result$polygon)
    operation <- if (result$buffer_m > 0.005) {
      "Expanded"
    } else if (result$buffer_m < -0.005) {
      "Shrank"
    } else {
      "Kept"
    }
    unit_label <- if (identical(input$target_area_unit, "acres")) "acres" else "ha"
    showNotification(
      sprintf(
        "%s AOI with a %.2f m buffer; area is %.3f %s.",
        operation,
        abs(result$buffer_m),
        result$achieved_area_m2 / factor,
        unit_label
      ),
      type = "message",
      duration = 6
    )
  })

  observeEvent(input$rotate_polygon, {
    poly_df <- drawn_polygon()
    if (is.null(poly_df) || nrow(poly_df) < 3) {
      showNotification("Please draw or import a polygon first!", type = "warning")
      return()
    }

    angle <- input$polygon_rotation_angle
    if (is.null(angle) || !is.numeric(angle) || length(angle) != 1 ||
        !is.finite(angle)) {
      showNotification("Enter a valid rotation angle.", type = "warning")
      return()
    }
    if (abs(angle %% 360) < 1e-10) {
      showNotification("The AOI is unchanged because the rotation is 0 degrees.", type = "message")
      return()
    }

    rotated_polygon <- tryCatch(
      rotate_polygon_about_center(poly_df, angle),
      error = function(error) {
        showNotification(
          paste("Could not rotate the polygon:", conditionMessage(error)),
          type = "error",
          duration = 7
        )
        NULL
      }
    )
    if (is.null(rotated_polygon)) return()

    drawn_polygon(rotated_polygon)
    replace_editable_aoi_on_map(rotated_polygon)
    direction_label <- if (angle > 0) "clockwise" else "counter-clockwise"
    showNotification(
      sprintf("Rotated AOI %.1f degrees %s.", abs(angle), direction_label),
      type = "message",
      duration = 5
    )
  })

  # Calculate number of photos (approximation)
  num_photos <- reactive({
    area_m2 <- polygon_area_m2()
    if (area_m2 <= 0) return(0)
    cam <- camera_params()
    altitude <- mission_altitude()
    footprint_width_m <- cam$footprint_width_ratio * altitude
    footprint_height_m <- cam$footprint_height_ratio * altitude
    effective_width_m <- footprint_width_m + max(0, input$transect_gap_m)
    effective_height_m <- capture_settings()$actual_spacing_m
    effective_area_per_photo <- effective_width_m * effective_height_m
    if (effective_area_per_photo <= 0) return(Inf)
    ceiling(area_m2 / effective_area_per_photo)
  })

  # --- Render the Mission Summary Output ---
  output$mission_summary <- renderUI({
    area_ha <- polygon_area_m2() / 10000
    area_m2 <- polygon_area_m2()
    speed <- mission_speed()
    cam <- camera_params()
    altitude <- mission_altitude()
    
    gsd_cm <- mission_gsd_cm()
    
    # Use the same generated and ordered route as the exports.
    plan_metrics <- NULL
    poly_df <- drawn_polygon()
    if (!is.null(poly_df) && nrow(poly_df) >= 3 && area_m2 > 0 && speed > 0) {
      plan_metrics <- build_plan_summary_data()$metrics
    }
    flight_time_min <- NA
    flight_distance_km <- NA
    batteries_needed <- NA_integer_
    if (!is.null(plan_metrics)) {
      flight_distance_km <- plan_metrics$route_distance_km
      flight_time_min <- plan_metrics$flight_time_min
      batteries_needed <- plan_metrics$batteries
    }

    capture <- capture_settings()
    photo_interval <- capture$interval
    transect_gap_m <- max(0, input$transect_gap_m)
    effective_sidelap <- -100 * transect_gap_m /
      (cam$footprint_width_ratio * altitude)
    
    metric_row <- function(label, value) {
      paste0(
        "<div class='mission-metric-row'>",
        "<span class='mission-metric-label'>", label, "</span>",
        "<span class='mission-metric-value'>", value, "</span>",
        "</div>"
      )
    }

    transect_warning <- if (!is.null(plan_metrics) &&
                            plan_metrics$oversized_transect_count > 0) {
      sprintf(
        paste0(
          "<div class='alert alert-warning' style='margin:10px 0 0;'>",
          "<strong>Line-of-sight warning:</strong> %d transect%s exceed%s 1.4 km ",
          "(longest %.2f km). Exports remain available. Enable trimming to shorten them.",
          "</div>"
        ),
        plan_metrics$oversized_transect_count,
        if (plan_metrics$oversized_transect_count == 1L) "" else "s",
        if (plan_metrics$oversized_transect_count == 1L) "s" else "",
        plan_metrics$longest_transect_km
      )
    } else ""

    HTML(paste0(
      "<div class='mission-metrics-grid'>",
        "<div class='mission-metric-section'>",
          "<div class='mission-metric-heading'>Flight</div>",
          metric_row("Drone", selected_drone()$file_tag),
          metric_row("Height above home", sprintf("%.1f m", altitude)),
          metric_row("Speed", sprintf("%.1f m/s", speed)),
          metric_row("GSD", if (!is.na(gsd_cm)) sprintf("%.2f cm/px", gsd_cm) else "N/A"),
          metric_row("Photo interval", if (!is.na(photo_interval)) sprintf("%.2f s", photo_interval) else "N/A"),
          metric_row("Achieved forward overlap", sprintf("%.1f%%", capture$achieved_overlap)),
        "</div>",
        "<div class='mission-metric-section'>",
          "<div class='mission-metric-heading'>Coverage</div>",
          metric_row("Area", sprintf("%.2f ha", area_ha)),
          metric_row("Footprint gap", sprintf("%.1f m (%.1f%% sidelap)", transect_gap_m, effective_sidelap)),
          metric_row("Transects", if (!is.null(plan_metrics)) prettyNum(plan_metrics$transect_count, big.mark = ",") else "N/A"),
          metric_row("Longest transect", if (!is.null(plan_metrics)) sprintf("%.2f km", plan_metrics$longest_transect_km) else "N/A"),
          metric_row(
            "1.4 km trimming",
            if (!isTRUE(input$trim_transect_length)) {
              "Off"
            } else if (!is.null(plan_metrics) && plan_metrics$trimmed_transect_count > 0) {
              sprintf(
                "%d trimmed (max was %.2f km)",
                plan_metrics$trimmed_transect_count,
                plan_metrics$original_longest_transect_km
              )
            } else {
              "On; none trimmed"
            }
          ),
          metric_row("Photos", if (!is.null(plan_metrics)) prettyNum(plan_metrics$photo_count, big.mark = ",") else "N/A"),
          metric_row("Distance", if (!is.na(flight_distance_km)) sprintf("%.2f km", flight_distance_km) else "N/A"),
          metric_row("Time", if (!is.na(flight_time_min)) sprintf("%.1f min", flight_time_min) else "N/A"),
          metric_row(
            sprintf("Batteries (%d min each)", assumed_battery_minutes),
            if (!is.na(batteries_needed)) sprintf("%d", batteries_needed) else "N/A"
          ),
        "</div>",
      "</div>",
      transect_warning
    ))
  })

  # --- Self-contained plan summary export ---
  output$download_plan_summary <- downloadHandler(
    filename = function() {
      paste0(
        "Megafauna_", selected_drone()$file_tag, "_Plan_Summary_",
        format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"
      )
    },
    contentType = "text/html",
    content = function(file) {
      plan_data <- build_plan_summary_data()
      if (plan_data$metrics$oversized_transect_count > 0) {
        showNotification(
          sprintf(
            "Warning: %d transect%s exceed%s 1.4 km. Summary export will continue.",
            plan_data$metrics$oversized_transect_count,
            if (plan_data$metrics$oversized_transect_count == 1L) "" else "s",
            if (plan_data$metrics$oversized_transect_count == 1L) "s" else ""
          ),
          type = "warning",
          duration = 10
        )
      }
      write_plan_summary_html(file, plan_data)
    }
  )
  
  # --- KML Export Handler (for AOI polygon only) ---
  output$download_kml <- downloadHandler(
    filename = function() {
      paste0("aoi_polygon_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".kml")
    },
    contentType = "application/vnd.google-earth.kml+xml",
    content = function(file) {
      poly_df <- drawn_polygon()
      
      # Ensure polygon is closed for KML
      if (!all(poly_df[1,] == poly_df[nrow(poly_df),])) {
        poly_df <- rbind(poly_df, poly_df[1,])
      }
      
      # Generate coordinate string for KML (lon,lat,alt format)
      coords_str <- paste(apply(poly_df, 1, function(row) {
        paste(row[1], row[2], "0", sep = ",")
      }), collapse = "\n            ")
      
      # Generate KML content
      kml_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Document>
    <name>AOI Polygon</name>
    <description>Megafauna survey area exported from Megafauna Drone Survey Planner</description>
    <Style id="AOI_Style">
      <LineStyle>
        <color>ff0000ff</color>
        <width>2</width>
      </LineStyle>
      <PolyStyle>
        <color>4d0000ff</color>
      </PolyStyle>
    </Style>
    <Placemark>
      <name>Survey Area</name>
      <styleUrl>#AOI_Style</styleUrl>
      <Polygon>
        <outerBoundaryIs>
          <LinearRing>
            <coordinates>
            %s
            </coordinates>
          </LinearRing>
        </outerBoundaryIs>
      </Polygon>
    </Placemark>
  </Document>
</kml>', coords_str)
      
      # Write to file
      writeLines(kml_content, file)
    }
  )
  
  # --- KMZ Export Handler ---
  output$download_kmz <- downloadHandler(
    filename = function() {
      paste0(
        "DJIWaypointMegafauna", selected_drone()$file_tag, "Mission",
        format(Sys.time(), "%Y%m%d-%H%M%S"), ".kmz"
      )
    },
    contentType = "application/vnd.google-earth.kmz",
    content = function(file) {
      poly_df <- drawn_polygon()
      
      # Require other parameters
      req(input$front_overlap, input$transect_gap_m)
      altitude <- mission_altitude()
      cam <- camera_params()
      drone_model <- selected_drone()
      speed <- mission_speed()
      angle <- if (is.null(input$transect_angle)) 0 else input$transect_angle
      
      # Generate transects and photo points
      res <- generate_transects_and_photopoints(
        poly_df, altitude, input$transect_gap_m, cam, angle,
        input$trim_transect_length
      )
      
      # Check if transects were generated
      if (length(res$transects) == 0) {
        showNotification("No transects generated. Check your polygon and parameters.", type = "error", duration = 5)
        req(FALSE)  # Stop the download
      }
      
      # Create temp directory for KMZ contents
      temp_dir <- tempfile()
      dir.create(temp_dir, recursive = TRUE)
      wpmz_dir <- file.path(temp_dir, "wpmz")
      dir.create(wpmz_dir, showWarnings = FALSE, recursive = TRUE)
      
      photo_interval <- capture_settings()$interval
      
      # Reorder transects based on start corner
      start_corner <- if (is.null(input$start_corner)) "top_left" else input$start_corner
      transects_ordered <- reorder_transects_by_start(res$transects, start_corner, angle)
      transects_ordered <- apply_reverse_transect_order(
        transects_ordered, input$reverse_transect_order
      )
      notify_oversized_transects(transects_ordered, "KMZ export")
      
      # Generate an editable DJI waypoint template with transect-scoped timed
      # photo actions (mapping2d cannot represent the requested zero sidelap).
      template_kml <- generate_template_kml(
        transects_ordered, altitude, speed, photo_interval, drone_model
      )
      writeLines(template_kml, file.path(wpmz_dir, "template.kml"))
      
      # Generate waylines.wpml with reordered transects
      waylines_wpml <- generate_waylines_wpml(
        transects_ordered, altitude, speed, photo_interval, drone_model
      )
      writeLines(waylines_wpml, file.path(wpmz_dir, "waylines.wpml"))
      
      # Create ZIP file (KMZ is just a ZIP) to a temp location first
      temp_kmz <- tempfile(fileext = ".kmz")
      zip::zip(
        zipfile = temp_kmz,
        files = c("wpmz/template.kml", "wpmz/waylines.wpml"),
        root = temp_dir,
        mode = "mirror",
        include_directories = FALSE
      )
      
      # Copy the ZIP file to the final destination
      file.copy(temp_kmz, file, overwrite = TRUE)
      
      # Clean up temp files
      unlink(temp_kmz)
      unlink(temp_dir, recursive = TRUE)
    }
  )
  
  # --- Litchi Plan Bundle Export Handler ---
  output$download_litchi_bundle <- downloadHandler(
    filename = function() {
      model <- selected_drone()
      paste0(
        "Megafauna_Litchi_", model$file_tag, "_Plan_Bundle_",
        format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"
      )
    },
    contentType = "application/zip",
    content = function(file) {
      bundle_dir <- tempfile("litchi_bundle_")
      dir.create(bundle_dir, recursive = TRUE)
      on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)

      poly_df <- drawn_polygon()
      
      # Require other parameters
      req(input$front_overlap, input$transect_gap_m)
      altitude <- mission_altitude()
      cam <- camera_params()
      drone_model <- selected_drone()
      speed <- mission_speed()
      angle <- if (is.null(input$transect_angle)) 0 else input$transect_angle
      
      # Generate transects and photo points
      res <- generate_transects_and_photopoints(
        poly_df, altitude, input$transect_gap_m, cam, angle,
        input$trim_transect_length
      )
      
      # Check if transects were generated
      if (length(res$transects) == 0) {
        showNotification("No transects generated. Check your polygon and parameters.", type = "error", duration = 5)
        req(FALSE)  # Stop the download
      }
      
      photo_interval <- capture_settings()$interval
      validate_model_capture_settings(
        drone_model, speed, photo_interval, "Litchi export"
      )
      
      # Reorder transects based on start corner
      start_corner <- if (is.null(input$start_corner)) "top_left" else input$start_corner
      transects_ordered <- reorder_transects_by_start(res$transects, start_corner, angle)
      transects_ordered <- apply_reverse_transect_order(
        transects_ordered, input$reverse_transect_order
      )
      notify_oversized_transects(transects_ordered, "Litchi export")
      
      # Flatten transects to waypoint list
      waypoints <- data.frame()
      waypoint_idx <- 1
      
      for (i in seq_along(transects_ordered)) {
        transect <- transects_ordered[[i]]
        n_points <- nrow(transect)
        
        # Calculate heading for this transect (direction from first to last point)
        if (n_points >= 2) {
          bearing <- geosphere::bearing(
            c(transect[1, 1], transect[1, 2]),
            c(transect[n_points, 1], transect[n_points, 2])
          )
          # Normalize bearing to 0-360
          if (bearing < 0) bearing <- bearing + 360
        } else {
          bearing <- 0
        }
        
        # Add all photo points in this transect
        for (j in 1:n_points) {
          wp <- data.frame(
            latitude = transect[j, 2],
            longitude = transect[j, 1],
            `altitude(m)` = altitude,
            `heading(deg)` = round(bearing),
            `curvesize(m)` = 0.2,
            rotationdir = 0,
            gimbalmode = 2,
            gimbalpitchangle = -90,
            actiontype1 = if (waypoint_idx == 1) 0 else -1,
            actionparam1 = if (waypoint_idx == 1) 10000 else 0,
            altitudemode = 0,
            `speed(m/s)` = speed,
            poi_latitude = 0,
            poi_longitude = 0,
            `poi_altitude(m)` = 0,
            poi_altitudemode = 0,
            photo_timeinterval = if (j < n_points) round(photo_interval, 2) else 0,  # Photo interval except at transect ends
            photo_distinterval = 0,
            check.names = FALSE
          )
          waypoints <- rbind(waypoints, wp)
          waypoint_idx <- waypoint_idx + 1
        }
        
        # Transition to next transect (if not last transect)
        # All waypoints in a transect keep the same heading (direction of the transect)
      }
      
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      csv_name <- paste0(
        "Megafauna_Survey_", drone_model$file_tag, "_", timestamp, ".csv"
      )
      lchz_name <- paste0(
        "Megafauna_Survey_", drone_model$file_tag, "_New_Hub_", timestamp, ".lchz"
      )
      notes_name <- "README_IMPORT.txt"

      validate_litchi_native_plan(waypoints, speed, drone_model)
      write.csv(waypoints, file.path(bundle_dir, csv_name), row.names = FALSE)
      native_plan <- litchi_native_plan()
      write_litchi_lchz(
        file.path(bundle_dir, lchz_name),
        native_plan,
        cruise_speed = speed,
        drone_model = drone_model
      )
      footprint_width <- cam$footprint_width_ratio * altitude
      footprint_height <- cam$footprint_height_ratio * altitude
      writeLines(
        c(
          "Megafauna Drone Survey Planner - Litchi Plan Bundle",
          "",
          paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
          paste0("Drone/camera profile: ", drone_model$label),
          paste0("Camera: ", drone_model$camera_note),
          paste0(
            "Image footprint at ", sprintf("%.1f", altitude), " m above home: ",
            sprintf("%.1f", footprint_width), " m wide x ",
            sprintf("%.1f", footprint_height), " m along-track."
          ),
          "",
          paste0("CSV: ", csv_name),
          "  Import into the New Litchi Hub and select the matching Litchi Pilot aircraft profile:",
          paste0("  ", drone_model$label, "."),
          "  Interval capture is enabled only on transect legs",
          "  and is switched off on connector legs between transects.",
          "",
          paste0("New Hub native plan: ", lchz_name),
          "  Import into https://hub.flylitchi.com/ via Flight Library > + > Import File.",
          "  Uses endpoint waypoints with per-segment interval overrides: On for transects, Off for connectors.",
          paste0("  Flight speed: ", sprintf("%.1f", speed), " m/s; photo interval: ", sprintf("%.2f", photo_interval), " s."),
          paste0("  Requested forward overlap: ", sprintf("%.1f", input$front_overlap),
                 "%; achieved after Litchi rounding: ", sprintf("%.1f", capture_settings()$achieved_overlap), "%"),
          paste0("  Gap between adjacent image footprints: ", sprintf("%.1f", max(0, input$transect_gap_m)), " m."),
          paste0(
            "  1.4 km transect trimming: ",
            if (isTRUE(input$trim_transect_length)) {
              route_metrics <- measure_transect_route(transects_ordered)
              sprintf(
                "enabled; %d oversized transect%s trimmed; longest exported transect %.2f km.",
                res$trimmed_transect_count,
                if (res$trimmed_transect_count == 1L) " was" else "s were",
                route_metrics$longest_transect_m / 1000
              )
            } else {
              route_metrics <- measure_transect_route(transects_ordered)
              oversized_count <- sum(
                route_metrics$transect_lengths_m > transect_length_limit_m + 1e-6
              )
              if (oversized_count > 0) {
                sprintf(
                  paste0(
                    "off; WARNING: %d transect%s exceed%s 1.4 km ",
                    "(longest %.2f km). Export was allowed; assess line of sight."
                  ),
                  oversized_count,
                  if (oversized_count == 1L) "" else "s",
                  if (oversized_count == 1L) "s" else "",
                  route_metrics$longest_transect_m / 1000
                )
              } else {
                "off; no transect exceeds 1.4 km."
              }
            }
          ),
          paste0(
            "  Transect order: ",
            if (isTRUE(input$reverse_transect_order)) "reversed (route flown backwards)." else "standard."
          ),
          "  The CSV format does not store the aircraft model; confirm the profile after import.",
          "  The native plan carries the route settings, but still confirm the displayed aircraft",
          "  profile in Litchi Hub before syncing it to Litchi Pilot.",
          "  M3E, M3T, M4E and M4T execution requires Litchi Pilot, not the older Litchi app.",
          "",
          "IMPORTANT: Before flying, verify the complete route, home point, altitude reference,",
          "speed, gimbal angle, photo interval overrides, obstacle clearance, return-to-home settings,",
          "aircraft compatibility, battery requirements, and all applicable aviation rules."
        ),
        file.path(bundle_dir, notes_name)
      )

      bundle_zip <- tempfile(fileext = ".zip")
      on.exit(unlink(bundle_zip, force = TRUE), add = TRUE)
      zip::zipr(
        zipfile = bundle_zip,
        files = c(csv_name, lchz_name, notes_name),
        root = bundle_dir,
        include_directories = FALSE
      )
      file.copy(bundle_zip, file, overwrite = TRUE)
    }
  )

}

# ==============================================================================
# Run the Application
# ==============================================================================
shinyApp(ui = ui, server = server)
