source("libraries.R")

walk(dir_ls("R"), source)

list(
  tar_target(
    broken_elevation,
    c("https://cdn.cyclingstage.com/images/tour-de-france/2022/stage-9-gpxroute.gpx")),
  tar_target(base_url, "https://www.cyclingstage.com/"),
  tar_target(
    editions, c(
      "tour-de-france-2022-gpx", "tour-de-france-2023-gpx",
      "tour-de-france-2024-gpx", "tour-de-france-2025-route")),
  tar_files(tdf_funs, dir_ls("R")),
  tar_target(links_css, ".data a"),
  tar_target(track_point_css, "trkpt"),
  tar_target(world, ne_countries(scale = "medium", returnclass = "sf")),
  
  tar_target(
    df_gpx_paths,
    verify(gpx_paths(base_url, editions, links_css), is_uniq(gpx_path))),
  tar_target(
    df_track_points, track_points(base_url, df_gpx_paths, track_point_css)),
  # tar_target(tbl_tdf_overview, tdf_overview_tbl(sf_tdf_stages)),
  tar_target(sf_tdf_stages, stages_sf(df_track_points)),
  # tar_target(leaflet_stages, stages_leaflet(sf_tdf_stages, "edition")),
  tar_target(gg_profile, vis_profile(sf_tdf_stages, broken_elevation)),
  tar_target(gg_tdf_stages, vis_tdf_stages(sf_tdf_stages, world)),
  
  tar_render(tour_de_france_map_post, "tour_de_france_map_post.Rmd")
)