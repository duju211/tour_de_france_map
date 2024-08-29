stages_leaflet <- function(sf_tdf_stages) {
  #factpal <- colorFactor(topo.colors(nrow(sf_tdf_stages)), sf_tdf_stages$start_finish)
  
  sf_tdf_stages |>
    st_zm() |>
    leaflet() |>
    addPolylines() |> #color = ~factpal(start_finish)
    addProviderTiles("Stadia.AlidadeSmooth")
}