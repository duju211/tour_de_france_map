tdf_overview_tbl <- function(sf_tdf_stages) {
  sf_tdf_stages |>
    as_tibble() |>
    select(where(negate(is_list))) |>
    reactable(
      groupBy = c("edition", "type"), selection = "multiple", filterable = TRUE,
      defaultSelected = seq_len(nrow(sf_tdf_stages)),
      columns = list(
        type = colDef(aggregate = "frequency"),
        km = colDef(aggregate = "sum", format = colFormat(digits = 2))))
}