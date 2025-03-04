prc_biome_recharge_io_herbier_benthos <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_io_herbier_benthos-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_io_herbier_benthos-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "io_herbiers_benthos_2023.csv",
  #     "io_biodiversity_2023_abondance_m2.csv",
  #     "io_biodiversity_2023_biomasse_m2.csv",
  #     "io_biodiversity_2023_classification.csv",
  #     "io_biodiversity_2023_presence.csv",
  #     "io_biodiversity_2023_stations.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  logbooks <- input_files[grepl("io_herbiers_benthos_2023.csv", input_files)] |>
    lapply(function(x) {
      vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        janitor::clean_names()
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      project_id = glue::glue("biome-{project_id}"),
      project_name = "Inventaires de biodiversité benthique à l'Île d'Orléans"
    )

  # Biodiversity
  stations <- input_files[grepl("io_biodiversity_2023_stations.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(station = stringr::str_replace_all(station, "[()]", ""))

  classification <- input_files[grepl("io_biodiversity_2023_classification.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::rename(
      phylum = embranchement,
      class = classe,
      order = ordre,
      family = famille,
      genus = genre,
      species = espece,
      scientific_name = noms_scientifiques
    )

  biodiversity <- list(
    occurrence = input_files[grepl("io_biodiversity_2023_presence.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::rename(scientific_name = x1) |>
      tidyr::pivot_longer(cols = -c(scientific_name), names_to = "station", values_to = "occurrence"),
    abondance = input_files[grepl("io_biodiversity_2023_abondance_m2.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::rename(scientific_name = x1) |>
      tidyr::pivot_longer(cols = -c(scientific_name), names_to = "station", values_to = "abundance_m2"),
    biomasse = input_files[grepl("io_biodiversity_2023_biomasse_m2.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::rename(scientific_name = x1) |>
      tidyr::pivot_longer(cols = -c(scientific_name), names_to = "station", values_to = "biomass_m2")
  ) |>
    purrr::reduce(dplyr::left_join, by = c("scientific_name", "station")) |>
    dplyr::mutate(
      station = toupper(station) |>
        stringr::str_replace_all(c("_" = "-", "L-A" = "L A", "L-B" = "L B"))
    ) |>
    dplyr::left_join(
      logbooks |>
        dplyr::select(project_id, parent_event_id, event_id, station),
      by = "station"
    ) |>
    dplyr::left_join(classification, by = "scientific_name") |>
    dplyr::rename(valid_name = scientific_name)


  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_io_herbier_benthos_logbooks.csv"), delim = ",")
  vroom::vroom_write(
    biodiversity,
    file.path(output_path, "biome_recharge_io_herbier_benthos_biodiversity.csv"),
    delim = ","
  )
}

prc_biome_recharge_lagrave <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_lagrave-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_lagrave-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "lg_biodiversity_2021.csv",
  #     "lg_biodiversity_2022.csv",
  #     "lg_biodiversity_2023.csv",
  #     "lg_logbook_2021.csv",
  #     "lg_logbook_2022.csv",
  #     "lg_logbook_2023.csv",
  #     "lg_logbook_2024.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  suppressWarnings({
    logbooks <- input_files[grepl("logbook", input_files)] |>
      lapply(function(x) {
        vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
          janitor::clean_names() |>
          dplyr::mutate(date = lubridate::as_date(date, format = "%y-%m-%d"))
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        project_id = glue::glue("biome-{project_id}"),
        project_name = "Suivi du site de recharge de plage de La Grave"
      )
  })

  # Occurrences
  suppressWarnings({
    biodiversity <- input_files[grepl("biodiversity", input_files)] |>
      lapply(function(x) {
        vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
          janitor::clean_names() |>
          dplyr::mutate(date = lubridate::as_date(date, format = "%y-%m-%d"))
      }) |>
      dplyr::bind_rows()
  })

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_lagrave_logbooks.csv"), delim = ",")
  vroom::vroom_write(biodiversity, file.path(output_path, "biome_recharge_lagrave_biodiversity.csv"), delim = ",")
}

prc_biome_recharge_pointe_aux_outardes <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_pointe-aux-outardes-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_pointe-aux-outardes-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "pao_biodiversity_2021.csv",
  #     "pao_biodiversity_2022.csv",
  #     "pao_biodiversity_2023.csv",
  #     "pao_logbook_2021.csv",
  #     "pao_logbook_2022.csv",
  #     "pao_logbook_2023.csv",
  #     "pao_logbook_2024.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  suppressWarnings({
    logbooks <- input_files[grepl("logbook", input_files)] |>
      lapply(function(x) {
        vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
          dplyr::rename_with(~ stringr::str_replace_all(.x, "\u00b5", "u")) |>
          janitor::clean_names() |>
          dplyr::mutate(
            couverture_sable = as.numeric(couverture_sable),
            couverture_vegetation = as.numeric(couverture_vegetation)
          )
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        project_id = glue::glue("biome-{project_id}"),
        project_name = "Suivi du site de recharge de plage de Pointe-aux-Outardes"
      )
  })

  # Occurrences
  suppressWarnings({
    biodiversity <- input_files[grepl("biodiversity", input_files)] |>
      lapply(function(x) {
        vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
          janitor::clean_names() #|>
        # dplyr::mutate(date = lubridate::as_date(date, format = "%y-%m-%d"))
      }) |>
      dplyr::bind_rows()
  })

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_pointe-aux-outardes_logbooks.csv"), delim = ",")
  vroom::vroom_write(biodiversity, file.path(output_path, "biome_recharge_pointe-aux-outardes_biodiversity.csv"), delim = ",")
}

prc_biome_recharge_maria <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_maria-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_maria-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "maria_logbook_2024.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  logbooks <- input_files[grepl("maria_logbook_2024.csv", input_files)] |>
    lapply(function(x) {
      vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        dplyr::rename_with(~ stringr::str_replace_all(.x, "\u00b5", "u")) |>
        janitor::clean_names()
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      project_id = glue::glue("biome-{project_id}"),
      project_name = "Suivi du site de recharge de plage de Maria"
    )

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_maria_logbooks.csv"), delim = ",")
}

prc_biome_recharge_pointe_aux_loups <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_pointe-aux-loups-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_pointe-aux-loups-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "pal_biodiversity_2023.csv",
  #     "pal_logbook_2023.csv",
  #     "pal_logbook_2024.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  logbooks <- input_files[grepl("logbook", input_files)] |>
    lapply(function(x) {
      vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        janitor::clean_names()
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      project_id = glue::glue("biome-{project_id}"),
      project_name = "Suivi du site de recharge de plage de Pointe aux Loups"
    )

  # Occurrences
  biodiversity <- input_files[grepl("biodiversity", input_files)] |>
    lapply(function(x) {
      vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        janitor::clean_names() #|>
      # dplyr::mutate(date = lubridate::as_date(date, format = "%y-%m-%d"))
    }) |>
    dplyr::bind_rows()

  # Export
  vroom::vroom_write(
    logbooks,
    file.path(output_path, "biome_recharge_pointe-aux-loups_logbooks.csv"),
    delim = ","
  )
  vroom::vroom_write(
    biodiversity,
    file.path(output_path, "biome_recharge_pointe-aux-loups_biodiversity.csv"),
    delim = ","
  )
}

prc_biome_recharge_sainte_flavie <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_sainte-flavie-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_sainte-flavie-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "sf_logbook_2024.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  suppressWarnings({
    logbooks <- input_files[grepl("sf_logbook_2024.csv", input_files)] |>
      lapply(function(x) {
        vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
          dplyr::rename_with(~ stringr::str_replace_all(.x, "\u00b5", "u")) |>
          janitor::clean_names()
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(couverture_vegetation = as.numeric(couverture_vegetation)) |>
      dplyr::mutate(
        project_id = glue::glue("biome-{project_id}"),
        project_name = "Suivi du site de recharge de plage de Sainte-Flavie"
      )
  })

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_sainte-flavie_logbooks.csv"), delim = ",")
}

prc_biome_recharge_sainte_luce <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/biome_recharge_sainte-luce-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/biome_recharge_sainte-luce-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "sl_logbook_2024.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Events
  suppressWarnings({
    logbooks <- input_files[grepl("sl_logbook_2024.csv", input_files)] |>
      lapply(function(x) {
        vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
          dplyr::rename_with(~ stringr::str_replace_all(.x, "\u00b5", "u")) |>
          janitor::clean_names()
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        project_id = glue::glue("biome-{project_id}"),
        project_name = "Suivi du site de recharge de plage de Sainte-Luce"
      )
  })

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_sainte-luce_logbooks.csv"), delim = ",")
}
