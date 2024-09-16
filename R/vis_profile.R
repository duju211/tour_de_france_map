vis_profile <- function(sf_tdf_stages, broken_elevation) {
  df_elev_profile <- sf_tdf_stages |>
    as_tibble() |>
    filter(!gpx_path %in% broken_elevation) |>
    select(gpx_path, elev_profile, edition) |>
    unnest(elev_profile)
  
  df_elev_profile |>
    ggplot(aes(x = distance, y = elevation, group = gpx_path, color = edition)) +
    geom_line()
}