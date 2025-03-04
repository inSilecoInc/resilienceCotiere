prc_inventaire_macroalgues_macroinvertebres <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/inventaire_macroalgues_macroinvertebres-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/inventaire_macroalgues_macroinvertebres-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "macroalgae-macroinvertebrates_event-data_2019.csv",
  #     "macroalgae-macroinvertebrates_occurrence-data_2019.csv",
  #     "macroalgae-macroinvertebrates_abiotic-measurement_2019.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("macroalgae-macroinvertebrates_event-data_2019.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(project_name = "Inventaire des macroalgues et des macroinvertébrés benthiques selon un gradient de stress")

  # Occurrences
  occurrences <- input_files[grepl("macroalgae-macroinvertebrates_occurrence-data_2019.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # abiotic
  abiotic <- input_files[grepl("macroalgae-macroinvertebrates_abiotic-measurement_2019.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",") |>
    janitor::clean_names()

  # Export
  vroom::vroom_write(events, file.path(output_path, "events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "occurrences.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "abiotic.csv"), delim = ",")
}
