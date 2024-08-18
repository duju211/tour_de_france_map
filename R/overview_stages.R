overview_stages <- function(edition_url) {
  host <- bow(edition_url)
  
  overview <- scrape(host, content = "text/html; charset=iso-8859-1")
  
  gpx_paths <- overview |>
    html_elements(".data a") |>
    html_attr("href")
  
  df_stage_raw <- overview |>
    html_element(".data") |>
    html_table() |>
    clean_names()
  
  df_stage <- df_stage_raw |>
    filter(!is.na(gpx)) |>
    add_column(gpx_url = gpx_paths)
  
  df_stage |>
    verify(is_uniq(gpx_url))
}