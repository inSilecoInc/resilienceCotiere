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

prc_zip_inventaire_marais_bergeronnes <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/zip_inventaire_marais_bergeronnes-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/zip_inventaire_marais_bergeronnes-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "marais_bergeronnes_events.csv",
  #     "marais_bergeronnes_occurrences.csv",
  #     "marais_bergeronnes_abiotic.csv",
  #     "marais_bergeronnes_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("marais_bergeronnes_events.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Occurrences
  occurrences <- input_files[grepl("marais_bergeronnes_occurrences.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Abiotic
  abiotic <- input_files[grepl("marais_bergeronnes_abiotic.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Abiotic
  abundance <- input_files[grepl("marais_bergeronnes_abundance.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Export
  vroom::vroom_write(events, file.path(output_path, "marais_bergeronnes_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "marais_bergeronnes_occurrences.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "marais_bergeronnes_abiotic.csv"), delim = ",")
  vroom::vroom_write(abundance, file.path(output_path, "marais_bergeronnes_abundance.csv"), delim = ",")
}

prc_zip_inventaire_marais_hickey <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/zip_inventaire_marais_hickey-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/zip_inventaire_marais_hickey-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "marais_hick_events.csv",
  #     "marais_hick_occurrences.csv",
  #     "marais_hick_abiotic.csv",
  #     "marais_hick_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  suppressMessages({
    events <- input_files[grepl("marais_hick_events.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Occurrences
  suppressWarnings({
    occurrences <- input_files[grepl("marais_hick_occurrences.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Abiotic
  abiotic <- input_files[grepl("marais_hick_abiotic.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Abiotic
  abundance <- input_files[grepl("marais_hick_abundance.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Export
  vroom::vroom_write(events, file.path(output_path, "marais_hick_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "marais_hick_occurrences.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "marais_hick_abiotic.csv"), delim = ",")
  vroom::vroom_write(abundance, file.path(output_path, "marais_hick_abundance.csv"), delim = ",")
}

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

prc_zip_inventaire_marais_pointe_fortin <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/zip_inventaire_marais_pointe_fortin-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/zip_inventaire_marais_pointe_fortin-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "marais_pointe_fortin_events.csv",
  #     "marais_pointe_fortin_abiotic.csv",
  #     "marais_pointe_fortin_occurrences.csv",
  #     "marais_pointe_fortin_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  events <- input_files[grepl("marais_pointe_fortin_events.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Occurrences
  occurrences <- input_files[grepl("marais_pointe_fortin_abiotic.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()

  # Abiotic
  suppressWarnings({
    abiotic <- input_files[grepl("marais_pointe_fortin_occurrences.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names()
  })

  # Abiotic
  abundance <- input_files[grepl("marais_pointe_fortin_abundance.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names()


  # Export
  vroom::vroom_write(events, file.path(output_path, "marais_pointe_fortin_events.csv"), delim = ",")
  vroom::vroom_write(occurrences, file.path(output_path, "marais_pointe_fortin_abiotic.csv"), delim = ",")
  vroom::vroom_write(abiotic, file.path(output_path, "marais_pointe_fortin_occurrences.csv"), delim = ",")
  vroom::vroom_write(abundance, file.path(output_path, "marais_pointe_fortin_abundance.csv"), delim = ",")
}

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
