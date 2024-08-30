stages_leaflet <- function(sf_tdf_stages, col_col) {
  df_tdf_stages <- as_tibble(sf_tdf_stages)
  df_tdf_count <- count(df_tdf_stages, .data[[col_col]])
  
  factpal <- colorFactor(
    topo.colors(nrow(df_tdf_count)), pull(df_tdf_stages, .data[[col_col]]))
  
  sf_tdf_stages |>
    st_zm() |>
    leaflet() |>
    addPolylines(color = ~factpal(pull(df_tdf_stages, .data[[col_col]]))) |>
    addProviderTiles("Stadia.AlidadeSmooth")
}