ana_indicateurs_ecosystemes <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/indicateurs_ecosystemes-1.0.0"
  # input_path <- "workspace/data/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     ""
  #    )
  # )
  input_files <- unlist(input_files)

  # Events_ecosystems
  events_ecosystems <- get_tbl(input_files, "event_ecosystem")

  # Ecosystems
  ecosystems <- get_tbl(input_files, "ecosystems")

  # Indicators
  indicators <- input_files[grepl("indicateurs-1.0.0", input_files)] |>
    lapply(vroom::vroom, progress = FALSE, show_col_types = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::left_join(
      events_ecosystems,
      by = "event_id",
      relationship = "many-to-many"
    ) |>
    na.omit() |>
    dplyr::left_join(ecosystems, by = "ecosystem_id", relationship = "many-to-many")

  # Export
  vroom::vroom_write(indicators, file.path(output_path, "indicateurs_ecosystemes.csv"), delim = ",")
}
