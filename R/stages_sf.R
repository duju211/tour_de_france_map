stages_sf <- function(df_tdf_stages, df_tdf_overview_pro) {
  df_tdf_stages |>
    filter(!is.na(elevation)) |>
    st_as_sf(coords = c("lon", "lat", "elevation"), crs = st_crs(4326)) |>
    summarise(geometry = st_combine(geometry), .by = gpx_path) |>
    st_cast("LINESTRING") |>
    left_join(
      df_tdf_overview_pro,
      by = join_by(gpx_path == gpx_url), relationship = "one-to-one")
}