stages_sf <- function(df_tdf_stages, df_tdf_overview_pro) {
  sf_points <- df_tdf_stages |>
    filter(!is.na(elevation)) |>
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
  
  sf_multipoints <- sf_points |>
    group_by(gpx_path) |>
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
  
  sf_multipoints |>
    st_cast("LINESTRING") |>
    left_join(
      df_tdf_overview_pro,
      by = join_by(gpx_path == gpx_url), relationship = "one-to-one")
}