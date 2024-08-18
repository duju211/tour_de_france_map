stages_sf <- function(df_track_points) {
  df_track_points |>
    filter(!is.na(elevation)) |>
    st_as_sf(coords = c("lon", "lat", "elevation"), crs = st_crs(4326)) |>
    summarise(geometry = st_combine(geometry), .by = gpx_path) |>
    st_cast("LINESTRING") |>
    mutate(gpx_path = as_factor(gpx_path))
}