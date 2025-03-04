prc_inventaire_pointe_john <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/inventaire_pointe_john-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/inventaire_pointe_john-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "events_pointe_john.csv",
  #     "occurrences_pointe_john.csv",
  #     "physico_chem_pointe_john.csv",
  #     "species_recovery_pointe_john.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("events_pointe_john.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(project_name = "Inventaires annuels de poissons et de macroinvertébrés benthiques de la zone médiolittorale près de la Pointe à John")

  # Occurrences
  occurrences <- input_files[grepl("occurrences_pointe_john.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Physico chemistry
  physico <- input_files[grepl("physico_chem_pointe_john.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Species recovery
  species_recovery <- input_files[grepl("species_recovery_pointe_john.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Export
  vroom::vroom_write(events, file.path(output_path, "events_pointe_john.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "occurrences_pointe_john.csv"), delim = ",")
  vroom::vroom_write(physico, file.path(output_path, "physico_chem_pointe_john.csv"), delim = ",")
  vroom::vroom_write(species_recovery, file.path(output_path, "species_recovery_pointe_john.csv"), delim = ",")
}
