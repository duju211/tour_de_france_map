source("libraries.R")

walk(dir_ls("R"), source)

df_editions <- tibble(
  year = c(2022L, 2023L, 2024L),
  edition_url = str_glue(
    "https://www.cyclingstage.com/tour-de-france-{year}-gpx/")) |>
  bind_rows(tibble(
    year = 2025,
    edition_url = "https://www.cyclingstage.com/tour-de-france-2025-route/"))

list(
  tar_map(
    values = df_editions, names = "year",
    tar_target(df_overview_stages, overview_stages(edition_url)),
    tar_target(
      df_track_points, command = {
        host <- bow(edition_url);
        track_points(host, df_overview_stages[["gpx_url"]])
      },
      pattern = map(df_overview_stages)),
    tar_target(sf_stages, stages_sf(df_track_points)),
    tar_target(leaflet_stages, stages_leaflet(sf_stages))
  )
)