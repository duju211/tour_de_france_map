source("libraries.R")

sf_stages <- tar_read(sf_stages_2025) 

factpal <- colorFactor(topo.colors(nrow(sf_stages)), sf_stages$gpx_path)

sf_stages |>
  st_zm() |>
  leaflet() |>
  addPolylines(color = ~factpal(gpx_path)) |>
  addProviderTiles(providers$OpenStreetMap)

leaflet(sf_stages$geometry[[1]]) |>
  addPolylines() |>
  addTiles()

leaflet(slice(sf_stages, 13)) |>
  addPolylines() |>
  addTiles()
