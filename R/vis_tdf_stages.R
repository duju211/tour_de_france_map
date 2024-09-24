vis_tdf_stages <- function(sf_tdf_stages, world) {
  tdf_bbox <- st_bbox(sf_tdf_stages)
  
  ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sf_tdf_stages, mapping = aes(color = edition)) +
    coord_sf(
      xlim = c(tdf_bbox[["xmin"]], tdf_bbox[["xmax"]]),
      ylim = c(tdf_bbox[["ymin"]], tdf_bbox[["ymax"]])) +
    theme_minimal() +
    labs(
      title = str_glue(
        "TdF Stages {str_flatten(unique(sf_tdf_stages$edition), ",
        "', ', ' and ')}"),
      color = "Edition",
      subtitle = "Color indicates Year") +
    theme(legend.position = "bottom")
}