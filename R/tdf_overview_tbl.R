tdf_overview_tbl <- function(sf_tdf_stages) {
  df_stages <- sf_tdf_stages |>
    as_tibble() |>
    select(where(negate(is_list)))
  
  df_stages |>
    select(-c(x, start_finish)) |>
    reactable(
      groupBy = c("edition", "type"), selection = "multiple", filterable = TRUE,
      defaultSelected = seq_len(nrow(sf_tdf_stages)),
      columns = list(
        gpx_path = colDef(cell = function(value, index) {
          htmltools::tags$a(
            href = value, target = "_blank",
            as.character(df_stages[index, "start_finish"]))
        }),
        type = colDef(aggregate = "frequency"),
        km = colDef(aggregate = "sum", format = colFormat(digits = 2))))
}