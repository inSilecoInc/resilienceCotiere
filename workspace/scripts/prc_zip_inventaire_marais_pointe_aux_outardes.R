prc_zip_inventaire_marais_pointe_aux_outardes <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/zip_inventaire_marais_pointe_aux_outardes-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/zip_inventaire_marais_pointe_aux_outardes-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "marais_pao_events.csv",
  #     "marais_pao_occurrences.csv",
  #     "marais_pao_abiotic.csv",
  #     "marais_pao_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("marais_pao_events.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Occurrences
  suppressWarnings({
    occurrences <- input_files[grepl("marais_pao_occurrences.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Abiotic
  abiotic <- input_files[grepl("marais_pao_abiotic.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Abiotic
  abundance <- input_files[grepl("marais_pao_abundance.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Export
  vroom::vroom_write(events, file.path(output_path, "marais_pao_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "marais_pao_occurrences.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "marais_pao_abiotic.csv"), delim = ",")
  vroom::vroom_write(abundance, file.path(output_path, "marais_pao_abundance.csv"), delim = ",")
}
