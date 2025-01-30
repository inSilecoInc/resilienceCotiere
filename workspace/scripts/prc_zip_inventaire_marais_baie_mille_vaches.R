prc_zip_inventaire_marais_baie_mille_vaches <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/zip_inventaire_marais_baie_mille_vaches-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/zip_inventaire_marais_baie_mille_vaches-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "DwC_marais_bmv_2019-2021.zip"
  #    )
  # )
  input_files <- unlist(input_files)

  # Unzip
  tmp <- file.path(output_path, "tmp")
  archive::archive_extract(input_files, tmp)
  input_files <- file.path(tmp, c(
    "zip-rne_marais-bmv_event_2019-2021.csv",
    "zip-rne_marais-bmv_event_emof_2019-2021.csv",
    "zip-rne_marais-bmv_occurrence_2019-2021.csv",
    "zip-rne_marais-bmv_occurrence-emof_2019-2021.csv"
  ))

  # Events
  events <- input_files[grepl("zip-rne_marais-bmv_event_2019-2021.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Occurrences
  occurrences <- input_files[grepl("zip-rne_marais-bmv_occurrence_2019-2021.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Abiotic
  abiotic <- input_files[grepl("zip-rne_marais-bmv_event_emof_2019-2021.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Abiotic
  abundance <- input_files[grepl("zip-rne_marais-bmv_occurrence-emof_2019-2021.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Export
  vroom::vroom_write(events, file.path(output_path, "marais_bmv_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "marais_bmv_occurrences.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "marais_bmv_abiotic.csv"), delim = ",")
  vroom::vroom_write(abundance, file.path(output_path, "marais_bmv_abundance.csv"), delim = ",")

  # Clean up temporary files
  fs::dir_delete(tmp)
}
