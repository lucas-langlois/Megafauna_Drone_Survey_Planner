// Drag handle between the sidebar and the map. The chosen width is kept in
// localStorage; double-click the handle to restore the default layout.
$(function () {
  var sidebar = $("#planner_sidebar").parent();
  var main = sidebar.next();
  var row = sidebar.parent();
  if (!sidebar.length || !main.length) return;

  var storageKey = "planner-sidebar-width";
  var minWidth = 260;
  var savedWidth = null;
  try { savedWidth = parseInt(window.localStorage.getItem(storageKey), 10) || null; } catch (e) {}

  row.css("position", "relative");
  var handle = $('<div class="sidebar-resizer" role="separator" aria-orientation="vertical" ' +
    'title="Drag to resize the panel. Double-click to reset."></div>').appendTo(row);

  function wide() { return window.innerWidth >= 768; }

  function refreshMap() {
    var widget = window.HTMLWidgets && HTMLWidgets.find("#map");
    if (widget && widget.getMap) widget.getMap().invalidateSize();
  }

  function placeHandle() {
    if (!wide()) { handle.hide(); return; }
    handle.show().css("left", (sidebar.outerWidth() - 4) + "px");
  }

  function apply(width) {
    if (!wide() || width === null) {
      sidebar.css("width", "");
      main.css("width", "");
    } else {
      var total = row.width();
      width = Math.max(minWidth, Math.min(width, total * 0.6));
      sidebar.css("width", width + "px");
      main.css("width", (total - width) + "px");
    }
    placeHandle();
    refreshMap();
  }

  handle.on("mousedown", function (event) {
    event.preventDefault();
    var rowLeft = row.offset().left;
    $("body").addClass("sidebar-resizing");
    $(document).on("mousemove.sidebarResize", function (move) {
      savedWidth = move.pageX - rowLeft;
      apply(savedWidth);
    });
    $(document).on("mouseup.sidebarResize", function () {
      $(document).off(".sidebarResize");
      $("body").removeClass("sidebar-resizing");
      try { window.localStorage.setItem(storageKey, String(Math.round(savedWidth))); } catch (e) {}
    });
  });

  handle.on("dblclick", function () {
    savedWidth = null;
    try { window.localStorage.removeItem(storageKey); } catch (e) {}
    apply(null);
  });

  $(window).on("resize", function () { apply(savedWidth); });
  apply(savedWidth);
  // The map widget may render after this script runs.
  setTimeout(function () { apply(savedWidth); }, 500);
});
