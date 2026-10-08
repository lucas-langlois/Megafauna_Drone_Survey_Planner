(function () {
  "use strict";

  var activeAttemptId = null;

  function report(attemptId, layerId, state, message) {
    if (!window.Shiny) return;
    Shiny.setInputValue("map_local_geotiff_status", {
      attemptId: attemptId,
      layerId: layerId,
      state: state,
      message: message || null,
      nonce: Math.random()
    }, {priority: "event"});
  }

  function rgba(values, mins, maxs, noDataValue) {
    if (!values || values.length < 1 || values[0] == null) {
      return null;
    }
    var colorBandCount = values.length >= 3 ? 3 : 1;
    if (noDataValue != null && Number.isFinite(Number(noDataValue))) {
      var numericNoData = Number(noDataValue);
      var allColorBandsAreNoData = values.slice(0, colorBandCount).every(
        function (value) { return Number(value) === numericNoData; }
      );
      if (allColorBandsAreNoData) return null;
    }
    if (!Array.isArray(mins) && Number.isFinite(Number(mins))) mins = [Number(mins)];
    if (!Array.isArray(maxs) && Number.isFinite(Number(maxs))) maxs = [Number(maxs)];
    function channel(value, index) {
      if (value == null || !Number.isFinite(Number(value))) return null;
      var numeric = Number(value);
      var minimum = mins && Number.isFinite(Number(mins[index])) ? Number(mins[index]) : 0;
      var maximum = maxs && Number.isFinite(Number(maxs[index])) ? Number(maxs[index]) : 255;
      if (minimum !== 0 || maximum !== 255) {
        numeric = maximum > minimum
          ? 255 * (numeric - minimum) / (maximum - minimum)
          : (numeric >= 0 && numeric <= 255 ? numeric : 128);
      }
      return Math.max(0, Math.min(255, Math.round(numeric)));
    }
    var red = channel(values[0], 0);
    var green = values.length >= 3 ? channel(values[1], 1) : red;
    var blue = values.length >= 3 ? channel(values[2], 2) : red;
    if (red == null || green == null || blue == null) return null;
    var alphaIndex = values.length === 2 ? 1 : (values.length > 3 ? 3 : -1);
    var alphaValue = 255;
    if (alphaIndex >= 0) {
      var rawAlpha = Number(values[alphaIndex]);
      if (!Number.isFinite(rawAlpha)) {
        alphaValue = 255;
      } else {
        var alphaMinimum = mins && Number.isFinite(Number(mins[alphaIndex]))
          ? Number(mins[alphaIndex]) : 0;
        var alphaMaximum = maxs && Number.isFinite(Number(maxs[alphaIndex]))
          ? Number(maxs[alphaIndex]) : 255;
        alphaValue = alphaMaximum === alphaMinimum
          ? (rawAlpha > 0 ? 255 : 0)
          : channel(rawAlpha, alphaIndex);
      }
    }
    var alpha = alphaValue == null ? 1 : alphaValue / 255;
    if (alpha === 0) return null;
    return "rgba(" + red + "," + green + "," + blue + "," + alpha + ")";
  }

  Shiny.addCustomMessageHandler("setLocalGeotiffAttempt", function (detail) {
    activeAttemptId = detail && detail.attemptId != null
      ? Number(detail.attemptId)
      : null;
  });

  Shiny.addCustomMessageHandler("localBasemapProgress", function (detail) {
    var container = document.getElementById("local-basemap-progress");
    var bar = document.getElementById("local-basemap-progress-bar");
    var text = document.getElementById("local-basemap-progress-text");
    if (!container || !bar || !text) return;

    if (detail.visible === false) {
      container.style.display = "none";
      return;
    }
    container.style.display = "block";
    var percent = Math.max(0, Math.min(100, Number(detail.percent) || 0));
    bar.style.width = percent + "%";
    bar.setAttribute("aria-valuenow", String(percent));
    bar.className = "progress-bar progress-bar-striped" +
      (detail.active === false ? "" : " active") +
      (detail.error ? " progress-bar-danger" : "");
    text.textContent = detail.text || "Loading local basemap...";
  });

  LeafletWidget.methods.addLocalGeotiffUrl = function (
      url, group, layerId, resolution, attemptId, mins, maxs) {
    var map = this;
    activeAttemptId = Number(attemptId);
    report(attemptId, layerId, "opening");
    var absoluteUrl = new URL(url, window.location.href).href;

    Promise.resolve(parseGeoraster(absoluteUrl)).then(function (georaster) {
      if (activeAttemptId !== Number(attemptId)) return;
      report(attemptId, layerId, "parsed");
      // geotiff.js discovers overview count by probing one image past the end.
      // Without caching, every visible Leaflet tile repeats that asynchronous
      // probe during zoom and pan. Share one count promise per source instead.
      if (georaster._geotiff &&
          typeof georaster._geotiff.getImageCount === "function") {
        var originalGetImageCount = georaster._geotiff.getImageCount.bind(georaster._geotiff);
        var imageCountPromise = null;
        georaster._geotiff.getImageCount = function () {
          if (!imageCountPromise) imageCountPromise = originalGetImageCount();
          return imageCountPromise;
        };
      }
      var commonLayerOptions = {
        georaster: georaster,
        debugLevel: 0,
        pixelValuesToColorFn: function (values) {
          return rgba(
            values,
            Array.isArray(mins) && mins.length ? mins : georaster.mins,
            Array.isArray(maxs) && maxs.length ? maxs : georaster.maxs,
            georaster.noDataValue
          );
        },
        opacity: 1,
        pane: "tilePane",
        // Avoid decoding throw-away intermediate tiles during animated zooms
        // and pans. The final full-resolution view is rendered on interaction
        // end, while nearby completed tiles remain buffered for smoothness.
        updateWhenZooming: false,
        updateWhenIdle: true,
        updateInterval: 500,
        keepBuffer: 3
      };
      var previewLayer = new GeoRasterLayer(Object.assign(
        {},
        commonLayerOptions,
        {resolution: Math.min(64, resolution)}
      ));
      var layer = new GeoRasterLayer(Object.assign(
        {},
        commonLayerOptions,
        {resolution: resolution}
      ));
      var completed = false;
      var loadTimeout = window.setTimeout(function () {
        if (!completed && activeAttemptId === Number(attemptId)) {
          report(attemptId, layerId, "error", "Timed out while rendering visible raster blocks");
        }
      }, 120000);
      layer.once("load", function () {
        if (completed || activeAttemptId !== Number(attemptId)) return;
        completed = true;
        window.clearTimeout(loadTimeout);
        report(attemptId, layerId, "ready");
      });
      layer.on("tileerror", function (event) {
        if (completed || activeAttemptId !== Number(attemptId)) return;
        var reason = event && event.error ? event.error.message : "Tile render failed";
        // GeoRasterLayer may retry a lower overview after a tile error. Treat
        // individual tile failures as warnings; the layer-level timeout above
        // reports a real failure if no visible raster blocks ever load.
        report(attemptId, layerId, "warning", reason);
      });
      // The inexpensive preview prevents a blank map while the full 256-sample
      // Leaflet tiles finish. The full layer is added second so it replaces the
      // preview as each detailed tile becomes available.
      if (activeAttemptId !== Number(attemptId)) return;
      map.layerManager.addLayer(
        L.layerGroup([previewLayer, layer]),
        "image",
        layerId,
        group
      );
    }).catch(function (error) {
      if (activeAttemptId !== Number(attemptId)) return;
      report(
        attemptId,
        layerId,
        "error",
        error && error.message ? error.message : String(error)
      );
    });
  };
})();
