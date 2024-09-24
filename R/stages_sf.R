stages_sf <- function(df_track_points) {
  sf_points <- df_track_points |>
    filter(!is.na(elevation)) |>
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
  
  sf_multipoints <- sf_points |>
    group_by(gpx_path, edition) |>
    mutate(
      distance_delta = as.numeric(st_distance(
        geometry, lag(geometry), by_element = TRUE))) |>
    mutate(
      distance = cumsum(if_else(is.na(distance_delta), 0, distance_delta)),
      .keep = "unused") |>
    summarise(
      geometry = st_combine(geometry),
      elev_profile = list(tibble(distance = distance, elevation = elevation)),
      .groups = "drop")
  
  st_cast(sf_multipoints, "LINESTRING")
}