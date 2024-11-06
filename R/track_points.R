track_points <- function(base_url, gpx_path_stages, track_point_css) {
  host <- bow(base_url)
  
  sessions <- map(gpx_path_stages, \(x) nod(host, x))
  
  gpx_html <- map(
    sessions,
    \(x) scrape(x, content = "text/html; charset=iso-8859-1", verbose = TRUE))
  
  track_points <- map(gpx_html, \(x) html_elements(x, track_point_css))
  
  list_track_points <- map(
    track_points, \(x) tibble(
      lat = html_attr(x, "lat"),
      lon = html_attr(x, "lon"),
      elevation = html_text(x)))
  
  df_track_points <- list_track_points |>
    set_names(gpx_path_stages) |>
    bind_rows(.id = "gpx_path") |>
    mutate(across(c(lat, lon, elevation), \(x) parse_number(x)))
}