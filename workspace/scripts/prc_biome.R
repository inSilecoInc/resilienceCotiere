prc_biome_recharge_io_herbier_benthos <- function(input_files, output_path) {
  input_files <- unlist(input_files)

  # Events
  logbooks <- input_files[grepl("io_herbiers_benthos_2023.csv", input_files)] |>
    lapply(function(x) {
      vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        janitor::clean_names()
    }) |>
    dplyr::bind_rows()

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_io_herbier_benthos_logbooks.csv"), delim = ",")
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
      dplyr::bind_rows()
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
      dplyr::bind_rows()
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
    dplyr::bind_rows()

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
    dplyr::bind_rows()

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
  logbooks <- input_files[grepl("sf_logbook_2024.csv", input_files)] |>
    lapply(function(x) {
      vroom::vroom(x, progress = FALSE, show_col_types = FALSE) |>
        dplyr::rename_with(~ stringr::str_replace_all(.x, "\u00b5", "u")) |>
        janitor::clean_names()
    }) |>
    dplyr::bind_rows()

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
      dplyr::bind_rows()
  })

  # Export
  vroom::vroom_write(logbooks, file.path(output_path, "biome_recharge_sainte-luce_logbooks.csv"), delim = ",")
}
