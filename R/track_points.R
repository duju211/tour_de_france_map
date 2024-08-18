track_points <- function(host, gpx_path) {
  gpx_html <- nod(host, gpx_path) |>
    scrape(content = "text/html; charset=iso-8859-1", verbose = TRUE)
  
  track_points <- html_elements(gpx_html, "trkpt")
  
  tibble(
    gpx_path = gpx_path,
    lat = html_attr(track_points, "lat"),
    lon = html_attr(track_points, "lon"),
    elevation = html_text(track_points)) |>
    mutate(across(c(lat, lon, elevation), \(x) parse_number(x)))
}