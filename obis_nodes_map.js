function initOBISMap(containerId, markerColor) {
  const map = new maplibregl.Map({
    container: containerId,
    style: {
      version: 8,
      sources: {
        coastlines: {
          type: "vector",
          tiles: ["https://tiles.obis.org/coastlines_tiles/{z}/{x}/{y}.pbf"],
          minzoom: 0,
          maxzoom: 14
        }
      },
      layers: [
        {id: "background", type: "background", paint: {"background-color": "#ffffff"}},
        {id: "coastlines", type: "line", source: "coastlines", "source-layer": "coastlines", 
         paint: {"line-color": "#000000", "line-width": 0.5}}
      ]
    },
    center: [0, 20],
    zoom: 1
  });
  
  fetch("https://api.obis.org/node")
    .then(r => r.json())
    .then(data => {
      const nodes = (data.results || []).filter(n => typeof n.lon === "number" && typeof n.lat === "number");
      
      map.on("load", function() {
        nodes.forEach(n => {
          new maplibregl.Marker({color: markerColor || "#156082", scale: 0.7})
            .setLngLat([n.lon, n.lat])
            .addTo(map);
        });
      });
    });
}
