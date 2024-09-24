gpx_paths <- function(base_url, editions, links_css) {
  host <- bow(base_url)
  
  sessions <- map(editions, \(x) nod(host, x))
  
  overview <- map(
    sessions,
    \(x) scrape(x, content = "text/html; charset=iso-8859-1", verbose = TRUE))
  
  link_nodes <- map(overview, \(x) html_elements(x, links_css))
  
  gpx_paths <- map(
    link_nodes, \(x) html_attr(x[html_text2(x) == "GPX"], "href"))
  
  tibble(url = map_chr(sessions, "url")) |>
    mutate(
      gpx_path = gpx_paths, edition = str_extract(url, "\\d+"),
      .keep = "none") |>
    unnest(gpx_path)
}