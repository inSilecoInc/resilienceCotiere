prc_yanick_gendreau <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/yanick_gendreau-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/yanick_gendreau-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "Liste_Sp_CindyGrant.txt",
  #     "yanick_intertidal_data_abundance.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Function to parse mixed date formats
  parse_mixed_dates <- function(date_str) {
    readr::parse_date(
      date_str,
      format = c("%d-%b-%y", "%Y-%m-%d"), # French abbreviated months & ISO format
      locale = readr::locale("fr") # Support French month names
    )
  }

  # Intertidal
  suppressWarnings({
    intertidal <- input_files[grepl("yanick_intertidal_data_abundance.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::mutate(
        lat_debut = stringr::str_replace_all(lat_debut, ",", ".") |> as.numeric(),
        lat_fin = stringr::str_replace_all(lat_fin, ",", ".") |> as.numeric(),
        lon_debut = stringr::str_replace_all(lon_debut, ",", ".") |> as.numeric(),
        lon_fin = stringr::str_replace_all(lon_fin, ",", ".") |> as.numeric(),
        maree_basse_m = stringr::str_replace_all(maree_basse_m, ",", ".") |> as.numeric(),
        project_id = "Gendreau_integration",
        project_name = "Intégration de données de Yanick Gendreau effectuée en 2016",
        event_date = dplyr::if_else(
          stringr::str_detect(date, "[a-zA-Z]"), # Detects month names
          readr::parse_date(as.character(date), format = "%d-%b-%y", locale = readr::locale("fr")),
          readr::parse_date(as.character(date), format = "%Y-%m-%d")
        ),
        biomasse_g = stringr::str_replace_all(biomasse_g, "[^0-9]", "") |>
          as.numeric()
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        longitude = mean(c(lon_debut, lon_fin), na.rm = TRUE),
        latitude = mean(c(lat_debut, lat_fin), na.rm = TRUE),
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(secteur, date, transect, latitude, longitude) |>
      dplyr::mutate(
        event_id = glue::glue("{secteur}_{event_date}_{transect}_{sprintf('%04d', dplyr::cur_group_id())}")
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        project_id, project_name, event_id, event_date, longitude, latitude,
        site_name = secteur,
        transect, forme_lintertidal, maree_basse_m, maree_basse_hr,
        heure_debut, distance_quadrat_m, substrat_quadrat, species,
        recouvrement_percent, biomasse_g, abondance, presence
      )
  })

  # Intertidal - subtidal
  suppressWarnings({
    intertidal_subtidal <- input_files[grepl("Liste_Sp_CindyGrant.txt", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = "\t") |>
      janitor::clean_names() |>
      dplyr::mutate(id_en_st = stringr::str_replace_all(id_en_st, "[^\\w\\s-]", ""))
  })

  # Export
  vroom::vroom_write(
    intertidal,
    file.path(output_path, "yanick_gendreau_intertidal.csv"),
    delim = ","
  )
  vroom::vroom_write(
    intertidal_subtidal,
    file.path(output_path, "yanick_gendreau_intertidal_subtidal.csv"),
    delim = ","
  )
}
