prc_zip_inventaire_marais_portneuf_sur_mer <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/zip_inventaire_marais_portneuf_sur_mer-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/zip_inventaire_marais_portneuf_sur_mer-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "marais_portneuf_sur_mer_events.csv",
  #     "marais_portneuf_sur_mer_occurrences.csv",
  #     "marais_portneuf_sur_mer_abiotic.csv",
  #     "marais_portneuf_sur_mer_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("marais_portneuf_sur_mer_events.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Occurrences
  occurrences <- input_files[grepl("marais_portneuf_sur_mer_occurrences.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Abiotic
  abiotic <- input_files[grepl("marais_portneuf_sur_mer_abiotic.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Abiotic
  abundance <- input_files[grepl("marais_portneuf_sur_mer_abundance.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Export
  vroom::vroom_write(events, file.path(output_path, "marais_portneuf_sur_mer_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "marais_portneuf_sur_mer_occurrences.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "marais_portneuf_sur_mer_abiotic.csv"), delim = ",")
  vroom::vroom_write(abundance, file.path(output_path, "marais_portneuf_sur_mer_abundance.csv"), delim = ",")
}
