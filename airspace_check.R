# Advisory airspace check against the bundled layer built by
# scripts/build_airspace_data.R. Covers CASA excluded-category / recreational
# rules (RPAS Platform Operating Rules v4.0) and DJI GEO zones, where DJI
# firmware stops the aircraft. Not CASA-verified: never report a plan as
# "clear".

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a

airspace_data_path <- file.path("data", "airspace_au.geojson")
airspace_meta_path <- file.path("data", "airspace_au_meta.json")
airspace_dji_group <- "DJI GEO zones (drone will stop)"
airspace_nofly_group <- "CASA no-fly zones (advisory)"
airspace_caution_group <- "CASA caution zones (advisory)"
airspace_height_group <- "CASA 300 ft zones (fly at 91 m or lower)"
airspace_gbrmpa_group <- "GBRMPA drone exclusion areas (permit needed)"
airspace_gbrmpa_path <- file.path("data", "gbrmpa_exclusions.geojson")

# A zone with a lower limit applies only above it. CASA-verified apps show the
# 300 ft (91.44 m) outer approach paths as "MAX 91M", so the limit is rounded
# down to whole metres: 91 m is allowed, 91.4 m is not.
airspace_zone_applies <- function(lower_ft, altitude_m) {
  lower_ft <= 0 | altitude_m > floor(lower_ft * 0.3048)
}

# Most restrictive first; findings are reported in this order.
airspace_severity <- c(
  dji_restricted = "dji-block", dji_authorization = "dji-block",
  gbrmpa_exclusion = "no-fly",
  prohibited = "no-fly", restricted = "no-fly",
  controlled_aerodrome = "no-fly", controlled_approach = "no-fly",
  controlled_approach_upper = "no-fly",
  # CASA: "Regardless of weight, stay clear of the runway approach and
  # departure paths"; CASA-verified apps show them as no-fly at every aerodrome.
  aerodrome_approach = "no-fly",
  aerodrome = "caution", heliport = "caution"
)
airspace_detail <- c(
  dji_restricted = "DJI GEO restricted zone. The drone will stop at the boundary and the mission will abort unless DJI grants a custom unlock.",
  dji_authorization = "DJI GEO authorization zone. Self-unlock it in DJI Pilot before the flight, or the drone will stop at the boundary.",
  gbrmpa_exclusion = "GBRMPA aircraft and drone exclusion area. No drone flights without specific Reef Authority permission.",
  prohibited = "Prohibited area. Do not enter.",
  restricted = "Restricted area. Do not enter without approval; check NOTAMs for activation.",
  controlled_aerodrome = "Within 3 NM (5.56 km) of a controlled aerodrome's runway. No-fly for drones over 250 g.",
  controlled_approach = "In the approach and departure paths of a controlled aerodrome. No-fly.",
  controlled_approach_upper = "In the outer approach and departure paths of a controlled aerodrome (7 to 8.5 km). Maximum 91 m (300 ft) here; fly at 91 m or lower.",
  aerodrome_approach = "In the approach and departure paths of a non-controlled aerodrome. No-fly: stay clear of runway approach and departure paths.",
  aerodrome = "Within 3 NM (5.56 km) of a non-controlled aerodrome. Allowed only if crewed aircraft are not operating there; if they are, move away and land. Stay outside the airfield boundary.",
  heliport = "Within 0.75 NM (1.39 km) of a helicopter landing site. Allowed only if no helicopter is operating there; if one is, move away and land."
)

load_airspace_layer <- function(path = airspace_data_path, meta_path = airspace_meta_path,
                                gbrmpa_path = file.path(dirname(path), basename(airspace_gbrmpa_path))) {
  layer <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(error) NULL)
  if (is.null(layer) || nrow(layer) == 0) return(NULL)
  gbrmpa <- tryCatch(sf::st_read(gbrmpa_path, quiet = TRUE), error = function(error) NULL)
  if (!is.null(gbrmpa) && nrow(gbrmpa)) {
    columns <- c("category", "name", "ident", "lower_ft", "lower_ref", "limits")
    layer <- rbind(layer[, columns], gbrmpa[, columns])
  }
  # Repair any geometry spherical (s2) predicates would reject, e.g. duplicate
  # vertices, so one bad polygon cannot break every check.
  bad <- !s2::s2_is_valid(sf::st_geometry(layer))
  if (any(bad)) sf::st_geometry(layer)[bad] <- sf::st_make_valid(sf::st_geometry(layer)[bad])
  meta <- tryCatch(jsonlite::read_json(meta_path), error = function(error) list())
  attr(layer, "airspace_meta") <- meta
  layer
}

airspace_data_label <- function(layer) {
  meta <- attr(layer, "airspace_meta")
  value <- function(x) if (is.null(x)) "unknown" else x
  sprintf("openAIP airspace updated %s; DJI zones and runways downloaded %s",
          value(meta$airspace_updated), value(meta$downloaded))
}

# aoi: two-column lon/lat matrix (closed or open ring).
# route: list of lon/lat matrices (transects, in flight order), may be empty.
# Returns list(status, findings data.frame, data_label).
# include_aoi = FALSE checks only the flown route (used when transects were
# clipped out of no-fly zones); a route touching a zone boundary, as clipped
# transect ends do, is not a finding.
check_airspace <- function(layer, aoi, route = list(), altitude_m = 120, include_aoi = TRUE) {
  empty <- data.frame(severity = character(), category = character(),
                      name = character(), detail = character(), stringsAsFactors = FALSE)
  if (is.null(layer)) {
    return(list(status = "not_performed", findings = empty,
                data_label = "Airspace data unavailable"))
  }
  aoi <- as.matrix(aoi)[, 1:2, drop = FALSE]
  if (!all(aoi[1, ] == aoi[nrow(aoi), ])) aoi <- rbind(aoi, aoi[1, ])
  # Separate missions are separate flights: no leg joins them.
  mission_of <- (attr(route, "airspace_route") %||% list())$mission
  if (length(mission_of) != length(route)) mission_of <- rep(1L, length(route))
  route_lines <- lapply(split(route, mission_of), function(part) {
    pts <- do.call(rbind, lapply(part, function(m) as.matrix(m)[, 1:2, drop = FALSE]))
    if (is.null(pts) || nrow(pts) < 2) NULL else sf::st_linestring(pts)
  })
  route_lines <- Filter(Negate(is.null), route_lines)
  has_route <- length(route_lines) > 0
  route_only <- !isTRUE(include_aoi) && has_route
  pieces <- if (route_only) list() else list(sf::st_polygon(list(aoi)))
  if (has_route) {
    # Includes the connectors between transects, in flight order.
    pieces <- c(pieces, route_lines)
  }
  plan <- sf::st_make_valid(sf::st_sfc(sf::st_geometrycollection(pieces), crs = 4326))

  # Zones starting above the flight height (e.g. the 300 ft outer approach
  # paths) do not apply.
  zones <- layer[layer$category %in% names(airspace_severity) &
                   airspace_zone_applies(layer$lower_ft, altitude_m), ]
  hits <- zones[lengths(sf::st_intersects(zones, plan)) > 0, ]
  if (route_only && nrow(hits)) {
    # Ignore boundary contact: keep zones the route runs inside for > 1 m.
    route_line <- sf::st_sfc(sf::st_multilinestring(lapply(route_lines, unclass)), crs = 4326)
    inside_m <- vapply(seq_len(nrow(hits)), function(k) {
      part <- suppressWarnings(sf::st_intersection(route_line, sf::st_geometry(hits)[k]))
      if (!length(part) || all(sf::st_is_empty(part))) 0 else sum(as.numeric(sf::st_length(part)))
    }, 0)
    hits <- hits[inside_m > 1, ]
  }
  if (!nrow(hits)) {
    return(list(status = "checked", findings = empty, data_label = airspace_data_label(layer)))
  }
  hits <- hits[order(match(hits$category, names(airspace_severity)), hits$name), ]
  findings <- data.frame(
    severity = unname(airspace_severity[hits$category]),
    category = hits$category,
    name = ifelse(nzchar(hits$ident) & !startsWith(hits$ident, "DJI"),
                  paste0(hits$name, " (", hits$ident, ")"), hits$name),
    detail = ifelse(hits$category %in% c("restricted", "prohibited"),
                    paste0(unname(airspace_detail[hits$category]), " Limits: ", hits$limits, "."),
             ifelse(hits$category == "gbrmpa_exclusion",
                    paste0(unname(airspace_detail[hits$category]), " Period: ", hits$limits, "."),
                    unname(airspace_detail[hits$category]))),
    stringsAsFactors = FALSE
  )
  list(status = "checked", findings = unique(findings), data_label = airspace_data_label(layer))
}

# Union of the zones the route must stay out of at this flight height: CASA
# no-fly zones and DJI GEO zones (the aircraft would stop there). Caution zones
# stay flyable. Returns NULL when none touch the given lon/lat extent.
airspace_forbidden_union <- function(layer, extent, altitude_m) {
  if (is.null(layer)) return(NULL)
  forbidden <- layer[airspace_severity[layer$category] %in% c("dji-block", "no-fly") &
                       airspace_zone_applies(layer$lower_ft, altitude_m), ]
  # Pad so straight east-west or north-south routes still give a valid box.
  extent <- unname(as.numeric(extent)) + c(-0.01, -0.01, 0.01, 0.01)
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = extent[1], ymin = extent[2],
                                     xmax = extent[3], ymax = extent[4]), crs = 4326))
  forbidden <- forbidden[lengths(sf::st_intersects(forbidden, box)) > 0, ]
  if (!nrow(forbidden)) return(NULL)
  sf::st_union(sf::st_make_valid(sf::st_geometry(forbidden)))
}

# Removes the parts of `lines` (sfc in a projected CRS, one per transect)
# inside `zone`, all lines at once. Untouched transects are returned as given;
# cut ones become their remaining pieces (lon/lat), dropping pieces shorter
# than min_length_m. Returns list(transects, affected, removed_m).
cut_lines_outside <- function(lines, zone, transects, min_length_m = 10) {
  out <- list(transects = transects, affected = 0L, removed_m = 0)
  crs <- sf::st_crs(lines)
  # Planar work without a CRS attached: sf otherwise re-parses the projection
  # for every predicate and length, which dominated the run time.
  lines <- sf::st_set_crs(lines, NA)
  zone <- sf::st_set_crs(zone, NA)
  hit <- lengths(sf::st_intersects(lines, zone)) > 0
  if (!any(hit)) return(out)
  planar_length <- function(m) sum(sqrt(rowSums(diff(m[, 1:2, drop = FALSE])^2)))
  hit_ids <- which(hit)
  kept <- suppressWarnings(sf::st_difference(lines[hit], zone))
  # sf drops lines that lie wholly inside the zone, so match each result back
  # to its line through the "idx" attribute rather than by position.
  kept_line <- attr(kept, "idx")[, 1]
  pieces_for <- vector("list", length(transects))
  for (i in hit_ids) pieces_for[[i]] <- list()
  removed <- sum(vapply(hit_ids, function(i) planar_length(sf::st_coordinates(lines[i])), 0))
  for (j in seq_along(kept)) {
    i <- hit_ids[kept_line[j]]
    geom <- kept[j]
    if (sf::st_is_empty(geom)) next
    coords <- sf::st_coordinates(suppressWarnings(sf::st_cast(
      sf::st_collection_extract(geom, "LINESTRING"), "LINESTRING")))
    part_id <- coords[, ncol(coords)]
    parts <- lapply(split(seq_len(nrow(coords)), part_id), function(ix) coords[ix, 1:2, drop = FALSE])
    part_m <- vapply(parts, planar_length, 0)
    parts <- parts[part_m >= min_length_m]
    removed <- removed - sum(part_m[part_m >= min_length_m])
    pieces_for[[i]] <- c(pieces_for[[i]], unname(parts))
  }
  # Back to lon/lat in one projection call.
  all_parts <- unlist(pieces_for, recursive = FALSE)
  if (length(all_parts)) {
    sizes <- vapply(all_parts, nrow, 0L)
    ll <- sf::sf_project(crs$wkt, "EPSG:4326", do.call(rbind, all_parts))
    ll_parts <- split(as.data.frame(ll), rep(seq_along(sizes), sizes))
    k <- 0
    for (i in which(hit)) {
      n <- length(pieces_for[[i]])
      pieces_for[[i]] <- if (n) lapply(ll_parts[(k + 1):(k + n)], function(d) unname(as.matrix(d))) else list()
      k <- k + n
    }
  }
  result <- list()
  for (i in seq_along(transects)) {
    result <- c(result, if (hit[i]) pieces_for[[i]] else list(transects[[i]]))
  }
  list(transects = result, affected = sum(hit), removed_m = removed)
}

# Removes the parts of each transect inside forbidden zones, splitting lines
# that cross a zone. Pieces shorter than min_length_m are dropped. The result
# carries attr "airspace_clip" = list(affected, removed_m).
# margin_m keeps clipped ends off the zone boundary, so the straight connector
# between neighbouring clipped ends does not cut inside a curved boundary.
clip_transects_outside_airspace <- function(layer, transects, altitude_m, min_length_m = 10,
                                            margin_m = 30) {
  info <- list(affected = 0L, removed_m = 0)
  if (!length(transects)) return(structure(transects, airspace_clip = info))
  all_points <- do.call(rbind, lapply(transects, function(m) as.matrix(m)[, 1:2, drop = FALSE]))
  extent <- unname(c(apply(all_points, 2, min), apply(all_points, 2, max)))
  zones <- airspace_forbidden_union(layer, extent, altitude_m)
  if (is.null(zones)) return(structure(transects, airspace_clip = info))

  centre <- colMeans(all_points)
  crs <- sprintf("+proj=aeqd +lat_0=%.8f +lon_0=%.8f +datum=WGS84 +units=m +no_defs",
                 centre[2], centre[1])
  zones_local <- sf::st_transform(zones, crs)
  # Only the part of the zones near the survey matters; cropping first keeps
  # the buffer and overlay cheap for large zones.
  pts_local <- sf::sf_project("EPSG:4326", crs, all_points)
  window <- c(apply(pts_local, 2, min) - 2000, apply(pts_local, 2, max) + 2000)
  zones_local <- suppressWarnings(sf::st_crop(zones_local, sf::st_bbox(
    c(xmin = window[1], ymin = window[2], xmax = window[3], ymax = window[4]), crs = sf::st_crs(crs))))
  zones_local <- sf::st_buffer(zones_local, margin_m)
  lines <- sf::st_transform(sf::st_sfc(lapply(transects, function(m) {
    sf::st_linestring(as.matrix(m)[, 1:2, drop = FALSE])
  }), crs = 4326), crs)
  cut <- cut_lines_outside(lines, sf::st_union(zones_local), transects, min_length_m)
  result <- cut$transects
  info$affected <- cut$affected
  info$removed_m <- cut$removed_m
  structure(result, airspace_clip = info)
}

# Re-orders clipped transects so the flown route never crosses a no-fly zone,
# without dropping any survey line. Transect pieces are grouped into blocks
# (neighbouring lines whose pieces line up with no zone between them); each
# block is flown back and forth as usual, and blocks are joined by the shortest
# leg, routed around the zone (via waypoints just outside it) when a straight
# leg would cross it. Detour waypoints are prepended to the next transect, so
# every exporter carries them. When no detour of at most max_detour_m extra
# exists, the next block starts a separate mission instead. `ordered` is the
# normal flight order; its first point is kept as the start. Returns the new
# order with attr "airspace_route" = list(blocks, detours, detour_m, missions,
# mission), where mission gives each transect's mission number.
route_around_zones <- function(layer, ordered, altitude_m, margin_m = 30, max_detour_m = 1500) {
  info <- list(blocks = 1L, detours = 0L, detour_m = 0, missions = 1L,
               mission = rep(1L, length(ordered)))
  done <- function(x) structure(x, airspace_route = info)
  if (length(ordered) < 2) return(done(ordered))
  all_ll <- do.call(rbind, lapply(ordered, function(m) as.matrix(m)[, 1:2, drop = FALSE]))
  extent <- unname(c(apply(all_ll, 2, min), apply(all_ll, 2, max)))
  zones <- airspace_forbidden_union(layer, extent + c(-0.05, -0.05, 0.05, 0.05), altitude_m)
  if (is.null(zones)) return(done(ordered))

  centre <- colMeans(all_ll)
  crs <- sprintf("+proj=aeqd +lat_0=%.8f +lon_0=%.8f +datum=WGS84 +units=m +no_defs",
                 centre[2], centre[1])
  # Project lists of matrices in one call each way.
  project_all <- function(mats, from, to) {
    sizes <- vapply(mats, nrow, 0L)
    out <- sf::sf_project(from, to, do.call(rbind, lapply(mats, function(m) as.matrix(m)[, 1:2, drop = FALSE])))
    ends <- cumsum(sizes)
    lapply(seq_along(mats), function(i) out[(ends[i] - sizes[i] + 1):ends[i], , drop = FALSE])
  }
  # Planar work without a CRS attached (re-parsing the projection for every
  # small predicate was the main cost).
  zones_xy <- sf::st_set_crs(sf::st_transform(zones, crs), NA)
  # Legs may run along a zone edge (clipped ends sit margin_m outside it);
  # only entering the zone itself counts.
  core <- sf::st_buffer(zones_xy, -2)
  leg_ok <- function(a, b) {
    !any(lengths(sf::st_intersects(sf::st_sfc(sf::st_linestring(rbind(a, b))), core)) > 0)
  }
  pieces <- project_all(ordered, "EPSG:4326", crs)
  crossing <- vapply(seq_len(length(pieces) - 1L), function(i) {
    !leg_ok(pieces[[i]][nrow(pieces[[i]]), ], pieces[[i + 1L]][1, ])
  }, TRUE)
  if (!any(crossing)) return(done(ordered))

  # --- Lines and blocks ---
  first <- pieces[[1]]
  u <- first[nrow(first), ] - first[1, ]
  u <- u / sqrt(sum(u^2))
  n <- c(-u[2], u[1])
  cross <- vapply(pieces, function(m) mean(m %*% n), 0)
  amin <- vapply(pieces, function(m) min(m %*% u), 0)
  amax <- vapply(pieces, function(m) max(m %*% u), 0)
  line_keys <- sort(unique(round(cross)))
  line_keys <- line_keys[c(TRUE, diff(line_keys) > 2)]
  line_of <- vapply(cross, function(x) which.min(abs(line_keys - x)), 0L)
  lines <- unname(split(seq_along(pieces), line_of))
  lines <- lapply(lines, function(ix) ix[order(amin[ix])])
  end_at <- function(i, at_max) {
    m <- pieces[[i]]
    along <- as.numeric(m %*% u)
    m[if (at_max) which.max(along) else which.min(along), ]
  }
  overlaps <- function(a, b) amin[a] < amax[b] && amin[b] < amax[a]

  block_of <- integer(length(pieces))
  blocks <- list()
  previous <- integer()
  for (k in seq_along(lines)) {
    for (p in lines[[k]]) {
      over_prev <- previous[vapply(previous, function(q) overlaps(q, p), TRUE)]
      joined <- FALSE
      if (length(over_prev) == 1L) {
        q <- over_prev
        over_next <- lines[[k]][vapply(lines[[k]], function(r) overlaps(q, r), TRUE)]
        if (length(over_next) == 1L &&
            leg_ok(end_at(q, FALSE), end_at(p, FALSE)) &&
            leg_ok(end_at(q, TRUE), end_at(p, TRUE))) {
          block_of[p] <- block_of[q]
          blocks[[block_of[q]]] <- c(blocks[[block_of[q]]], p)
          joined <- TRUE
        }
      }
      if (!joined) {
        blocks[[length(blocks) + 1L]] <- p
        block_of[p] <- length(blocks)
      }
    }
    previous <- lines[[k]]
  }
  info$blocks <- length(blocks)

  # Fly a block: lines in order (or reversed), starting at the min or max end.
  fly_block <- function(ix, reverse_lines, start_max) {
    if (reverse_lines) ix <- rev(ix)
    out <- vector("list", length(ix))
    at_max <- start_max
    for (j in seq_along(ix)) {
      m <- pieces[[ix[j]]]
      along <- as.numeric(m %*% u)
      out[[j]] <- m[order(along, decreasing = at_max), , drop = FALSE]
      at_max <- !at_max
    }
    out
  }
  options_for <- function(b) {
    out <- list()
    for (rl in c(FALSE, TRUE)) for (sm in c(FALSE, TRUE)) {
      out[[length(out) + 1L]] <- fly_block(blocks[[b]], rl, sm)
    }
    out
  }

  # --- Detours: shortest path via points just outside the zones ---
  all_xy <- do.call(rbind, pieces)
  box <- c(apply(all_xy, 2, min) - 3000, apply(all_xy, 2, max) + 3000)
  outline <- sf::st_simplify(sf::st_buffer(zones_xy, margin_m + 15), dTolerance = 10)
  nodes <- sf::st_coordinates(outline)[, 1:2, drop = FALSE]
  nodes <- unique(nodes[nodes[, 1] >= box[1] & nodes[, 1] <= box[3] &
                          nodes[, 2] >= box[2] & nodes[, 2] <= box[4], , drop = FALSE])
  nn <- nrow(nodes)
  visible <- function(from, to) {
    if (!nrow(to)) return(logical())
    segs <- sf::st_sfc(lapply(seq_len(nrow(to)), function(j) sf::st_linestring(rbind(from, to[j, ]))))
    lengths(sf::st_intersects(segs, core)) == 0
  }
  node_cost <- matrix(Inf, nn, nn)
  if (nn > 1) for (j in seq_len(nn - 1L)) {
    others <- (j + 1L):nn
    d <- sqrt(rowSums((nodes[others, , drop = FALSE] -
                         matrix(nodes[j, ], length(others), 2, byrow = TRUE))^2))
    node_cost[j, others] <- ifelse(visible(nodes[j, ], nodes[others, , drop = FALSE]), d, Inf)
    node_cost[others, j] <- node_cost[j, others]
  }
  path_between <- function(a, b) {
    if (leg_ok(a, b)) return(list(cost = sqrt(sum((b - a)^2)), via = matrix(numeric(), 0, 2)))
    if (!nn) return(list(cost = Inf, via = NULL))
    to_nodes <- function(p) {
      ifelse(visible(p, nodes), sqrt(rowSums((nodes - matrix(p, nn, 2, byrow = TRUE))^2)), Inf)
    }
    dist <- to_nodes(a)
    db <- to_nodes(b)
    prev <- rep(NA_integer_, nn)
    open <- rep(TRUE, nn)
    repeat {
      cand <- which(open & is.finite(dist))
      if (!length(cand)) break
      j <- cand[which.min(dist[cand])]
      open[j] <- FALSE
      better <- open & (dist[j] + node_cost[j, ] < dist)
      prev[better] <- j
      dist[better] <- dist[j] + node_cost[j, better]
    }
    total <- dist + db
    if (!any(is.finite(total))) return(list(cost = Inf, via = NULL))
    j <- which.min(total)
    via <- integer()
    while (!is.na(j)) {
      via <- c(j, via)
      j <- prev[j]
    }
    list(cost = min(total), via = nodes[via, , drop = FALSE])
  }

  # --- Join blocks greedily, starting where the normal route starts ---
  start_point <- pieces[[1]][1, ]
  first_options <- options_for(block_of[1])
  pick <- which.min(vapply(first_options, function(o) sum((o[[1]][1, ] - start_point)^2), 0))
  route <- first_options[[pick]]
  mission <- rep(1L, length(route))
  remaining <- setdiff(seq_along(blocks), block_of[1])
  while (length(remaining)) {
    here <- route[[length(route)]]
    here <- here[nrow(here), ]
    best <- NULL
    for (b in remaining) for (o in options_for(b)) {
      straight <- sqrt(sum((o[[1]][1, ] - here)^2))
      if (!is.null(best) && straight >= best$cost) next
      leg <- path_between(here, o[[1]][1, ])
      if (is.null(best) || leg$cost < best$cost) {
        best <- list(b = b, o = o, cost = leg$cost, via = leg$via, straight = straight)
      }
    }
    if (is.null(best) || !is.finite(best$cost) || best$cost - best$straight > max_detour_m) {
      # No reasonable legal join: the nearest block becomes a new mission,
      # flown as a separate flight.
      nearest <- NULL
      for (b in remaining) for (o in options_for(b)) {
        d <- sqrt(sum((o[[1]][1, ] - here)^2))
        if (is.null(nearest) || d < nearest$d) nearest <- list(b = b, o = o, d = d)
      }
      info$missions <- info$missions + 1L
      route <- c(route, nearest$o)
      mission <- c(mission, rep(info$missions, length(nearest$o)))
      remaining <- setdiff(remaining, nearest$b)
      next
    }
    if (nrow(best$via)) {
      info$detours <- info$detours + 1L
      info$detour_m <- info$detour_m + best$cost - best$straight
      best$o[[1]] <- rbind(best$via, best$o[[1]])
    }
    route <- c(route, best$o)
    mission <- c(mission, rep(info$missions, length(best$o)))
    remaining <- setdiff(remaining, best$b)
  }
  info$mission <- mission
  done(project_all(route, crs, "EPSG:4326"))
}

airspace_overlay_groups <- function(layer) {
  if (is.null(layer)) character() else
    c(airspace_dji_group, airspace_nofly_group, airspace_gbrmpa_group, airspace_height_group,
      airspace_caution_group)
}

add_airspace_layers <- function(map, layer) {
  if (is.null(layer)) return(map)
  severity <- airspace_severity[layer$category]
  meta <- attr(layer, "airspace_meta")
  attribution <- paste0(
    "<span title='", htmltools::htmlEscape(airspace_data_label(layer)), "'>",
    "Airspace (advisory): openAIP CC BY-NC-SA, &copy; OpenStreetMap ODbL, DJI FlySafe",
    if (any(layer$category == "gbrmpa_exclusion")) ", GBRMPA" else "",
    " &middot; ", htmltools::htmlEscape(meta$downloaded %||% ""), "</span>")
  # Non-interactive so clicks still reach the map (drawing, pilot placement);
  # the mission panel names any zone the plan touches.
  passive <- leaflet::pathOptions(interactive = FALSE)
  gbrmpa <- layer[layer$category == "gbrmpa_exclusion", ]
  if (nrow(gbrmpa)) {
    map <- map %>% leaflet::addPolygons(
      data = sf::st_zm(gbrmpa), group = airspace_gbrmpa_group,
      color = "#1b5e20", weight = 2, fillColor = "#43a047", fillOpacity = 0.25,
      options = passive
    )
  }
  map %>%
    leaflet::addPolygons(
      data = sf::st_zm(layer[severity %in% "caution", ]), group = airspace_caution_group,
      color = "#ffb300", weight = 2, dashArray = "6 4", fillColor = "#ffca28",
      fillOpacity = 0.18, options = passive
    ) %>%
    leaflet::addPolygons(
      data = sf::st_zm(layer[layer$category == "controlled_approach_upper", ]),
      group = airspace_height_group,
      color = "#0288d1", weight = 2, dashArray = "2 5", fillColor = "#4fc3f7",
      fillOpacity = 0.18, options = passive
    ) %>%
    leaflet::addPolygons(
      data = sf::st_zm(layer[severity %in% "no-fly" &
                               !layer$category %in% c("controlled_approach_upper", "gbrmpa_exclusion"), ]),
      group = airspace_nofly_group,
      color = "#c62828", weight = 1.5, fillColor = "#e53935", fillOpacity = 0.15,
      options = passive
    ) %>%
    leaflet::addPolygons(
      data = sf::st_zm(layer[severity %in% "dji-block", ]), group = airspace_dji_group,
      color = "#6a1b9a", weight = 2, fillColor = "#8e24aa", fillOpacity = 0.3,
      options = passive
    ) %>%
    # Credit the airspace sources in Leaflet's own attribution line, so the
    # two cannot overlap.
    htmlwidgets::onRender(sprintf(
      "function(el, x) { this.attributionControl.addAttribution(%s); }",
      jsonlite::toJSON(attribution, auto_unbox = TRUE)))
}

airspace_summary_text <- function(result) {
  if (identical(result$status, "not_performed")) {
    return("Airspace check not performed (data unavailable).")
  }
  count <- function(level) sum(result$findings$severity == level)
  if (nrow(result$findings) == 0) {
    return(sprintf("No intersections found (%s). Advisory only; confirm in a CASA-verified app and DJI FlySafe.",
                   result$data_label))
  }
  sprintf("%d DJI block, %d CASA no-fly, %d caution (%s). Advisory only; confirm in a CASA-verified app and DJI FlySafe.",
          count("dji-block"), count("no-fly"), count("caution"), result$data_label)
}
