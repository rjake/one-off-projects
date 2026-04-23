<script>
document.addEventListener('DOMContentLoaded', function() {
  var map = window.HTMLWidgets.getInstance(document.querySelector('.leaflet')).getMap();
  
  // Create a dedicated pane for the location marker
  var locationPane = map.createPane('locationPane');
  locationPane.style.zIndex = 650; // above overlayPane (400) and markerPane (600)

  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(function(pos) {
      var lat = pos.coords.latitude;
      var lng = pos.coords.longitude;
      L.circleMarker([lat, lng], {
        pane: 'locationPane',  // assign to high z-index pane
        radius: 6,
        color: 'white',
        fillColor: 'blue',
        fillOpacity: 1,
        weight: 1
      }).addTo(map).bindPopup('You are here');
      map.setView([lat, lng], 15);
    });
  }

  // --- Swap CircleMarkers to geographic Circles ---
  map.eachLayer(function(layer) {
    if (layer instanceof L.CircleMarker && !(layer instanceof L.Circle)) {
      var latlng = layer.getLatLng();
      var opts   = layer.options;
      var popup  = layer.getPopup();

      var circle = L.circle(latlng, {
        radius: 20,        // meters — adjust to taste
        color:       opts.color,
        fillColor:   opts.fillColor,
        fillOpacity: opts.fillOpacity,
        weight:      opts.weight
      });

      if (popup) circle.bindPopup(popup.getContent());
      circle.addTo(map);
      map.removeLayer(layer);
    }
  });
});
</script>
