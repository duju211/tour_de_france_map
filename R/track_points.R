track_points <- function(base_url, df_gpx_paths, track_point_css) {
  host <- bow(base_url)
  
  sessions <- map(pull(df_gpx_paths, gpx_path), \(x) nod(host, x))
  
  gpx_html <- map(
    sessions,
    \(x) scrape(x, content = "text/html; charset=iso-8859-1", verbose = TRUE))
  
  track_points <- map(gpx_html, \(x) html_elements(x, track_point_css))
  
  list_track_points <- map(
    track_points, \(x) tibble(
      lat = html_attr(x, "lat"),
      lon = html_attr(x, "lon"),
      elevation = html_text(x)))
  
  df_track_points <- df_gpx_paths |>
    mutate(df_track_points = list_track_points) |>
    unnest(df_track_points) |>
    mutate(across(c(lat, lon, elevation), \(x) parse_number(x)))
}