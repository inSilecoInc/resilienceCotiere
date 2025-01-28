prc_biodiversite_piei <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biodiversite_piei-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biodiversite_piei-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "piei_event_information_estuary_gulf_2017-2021.csv",
  #     "piei_taxon_occurrence_estuary_gulf_2017-2021.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("piei_event_information_estuary_gulf_2017-2021.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Occurrences
  suppressWarnings({
    occurrences <- input_files[grepl("piei_taxon_occurrence_estuary_gulf_2017-2021.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Export
  vroom::vroom_write(events, file.path(output_path, "events_piei.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "occurrences_piei.csv"), delim = ",")
}
