source("libraries.R")

walk(dir_ls("R"), source)

df_editions <- tibble(
  year = c(2022L, 2023L, 2024L),
  edition_url = str_glue(
    "https://www.cyclingstage.com/tour-de-france-{year}-gpx/")) |>
  bind_rows(tibble(
    year = 2025,
    edition_url = "https://www.cyclingstage.com/tour-de-france-2025-route/"))

stages_mapped <- tar_map(
  values = df_editions, names = "year",
  tar_target(df_overview_stages, overview_stages(edition_url)),
  tar_target(
    df_track_points, command = {
      host <- bow(edition_url);
      track_points(host, df_overview_stages[["gpx_url"]])
    },
    pattern = map(df_overview_stages)))

list(
  stages_mapped,
  tar_target(
    broken_elevation,
    c("https://cdn.cyclingstage.com/images/tour-de-france/2022/stage-9-gpxroute.gpx")),
  
  tar_combine(
    df_tdf_stages, stages_mapped[["df_track_points"]],
    command = dplyr::bind_rows(!!!.x, .id = "edition")),
  tar_combine(
    df_tdf_overview, stages_mapped[["df_overview_stages"]],
    command = dplyr::bind_rows(!!!.x, .id = "edition")),
  tar_target(df_tdf_overview_pro, tdf_overview_pro(df_tdf_overview)),
  tar_target(tbl_tdf_overview, tdf_overview_tbl(sf_tdf_stages)),
  tar_target(sf_tdf_stages, stages_sf(df_tdf_stages, df_tdf_overview_pro)),
  tar_target(
    rds_tdf_stages, command = {
      write_rds(sf_tdf_stages, "tdf_stages.rds");
      return("tdf_stages.rds")
    }, format = "file"),
  tar_target(leaflet_stages, stages_leaflet(sf_tdf_stages, "edition")),
  tar_target(gg_profile, vis_profile(sf_tdf_stages, broken_elevation))
)