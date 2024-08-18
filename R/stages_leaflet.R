stages_leaflet <- function(sf_stages) {
  factpal <- colorFactor(topo.colors(nrow(sf_stages)), sf_stages$gpx_path)
  
  sf_stages |>
    st_zm() |>
    leaflet() |>
    addPolylines(color = ~factpal(gpx_path)) |>
    addProviderTiles(providers$OpenStreetMap)
}