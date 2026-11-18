// Shows an "Updating plan…" badge over the map while the server recalculates.
// It appears only if the work takes longer than a moment, to avoid flicker.
$(function () {
  var badge = $('<div id="plan-busy" role="status" aria-live="polite">' +
    '<span class="plan-busy-spinner" aria-hidden="true"></span>Updating plan…</div>');
  var timer = null;

  function attach() {
    var map = $("#map");
    if (map.length && !badge.parent().is(map)) map.append(badge);
  }

  $(document).on("shiny:busy", function () {
    attach();
    if (timer) return;
    timer = setTimeout(function () { badge.addClass("visible"); }, 300);
  });

  $(document).on("shiny:idle", function () {
    clearTimeout(timer);
    timer = null;
    badge.removeClass("visible");
  });
});
