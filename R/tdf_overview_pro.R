tdf_overview_pro <- function(df_tdf_overview) {
  df_tdf_overview |>
    select(-x_2) |>
    mutate(edition = str_extract(edition, "\\d+$"))
}