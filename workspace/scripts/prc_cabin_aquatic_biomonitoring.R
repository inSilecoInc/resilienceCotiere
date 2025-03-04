prc_cabin_aquatic_biomonitoring <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/cabin_aquatic_biomonitoring-1.0.0/processed/"
  # input_path <- "workspace/data/harvested/cabin_aquatic_biomonitoring-1.0.0/raw/"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "cabin_benthic_data_maritimes.csv",
  #     "cabin_benthic_data_st_lawrence.csv",
  #     "cabin_habitat_data_maritimes.csv",
  #     "cabin_habitat_data_st_lawrence.csv",
  #     "cabin_study_data_maritimes.csv",
  #     "cabin_study_data_st_lawrence.csv"
  #    )
  # )
  input_files <- unlist(input_files)

  # Function to import the cabin data
  import_cabin <- function(input_path) {
    readr::read_lines(input_path, locale = readr::locale(encoding = "UTF-16LE")) |>
      stringr::str_replace_all('(?<!")\\,(?!")', " ") |> # Remove unnecessary quotes
      stringr::str_replace_all('\\\"', "") |> # Remove unnecessary quotes
      paste0(collapse = "\n") |>
      textConnection() |>
      read.csv(stringsAsFactors = FALSE, row.names = NULL) |> # Read as data.frame without factors
      dplyr::rename_with(~ gsub("^x\\.", "", .x)) |> # Remove the "X." prefix
      dplyr::rename_with(~ sub("\\..*", "", .x)) |> # Remove everything after the first `.`
      janitor::clean_names() |> # Clean column names
      readr::type_convert()
  }

  # Benthic
  suppressMessages({
    suppressWarnings({
      benthic <- list(
        input_files[grepl("cabin_benthic_data_maritimes.csv", input_files)] |>
          import_cabin() |>
          dplyr::mutate(bassin = "maritimes") |>
          dplyr::relocate(bassin),
        input_files[grepl("cabin_benthic_data_st_lawrence.csv", input_files)] |>
          import_cabin() |>
          dplyr::mutate(bassin = "st_lawrence") |>
          dplyr::relocate(bassin)
      ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
          event_id = glue::glue("{bassin}-{site}-{site_visit_id}")
        )
    })
  })

  # Habitat
  suppressMessages({
    suppressWarnings({
      habitat <- list(
        input_files[grepl("cabin_habitat_data_maritimes.csv", input_files)] |>
          import_cabin() |>
          dplyr::mutate(
            bassin = "maritimes",
            site_visit_id = as.character(site_visit_id)
          ) |>
          dplyr::relocate(bassin),
        input_files[grepl("cabin_habitat_data_st_lawrence.csv", input_files)] |>
          import_cabin() |>
          dplyr::mutate(
            bassin = "st_lawrence",
            site_visit_id = as.character(site_visit_id)
          ) |>
          dplyr::relocate(bassin)
      ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
          event_id = glue::glue("{bassin}-{site}-{site_visit_id}")
        )
    })
  })

  # Study
  # Function to import the cabin data
  import_cabin <- function(input_path) {
    readr::read_lines(input_path, locale = readr::locale(encoding = "UTF-16LE")) |>
      stringr::str_replace_all('\\\"', "'") |> # Remove unnecessary quotes
      stringr::str_replace_all("(?<!')\\,(?!')", "") |>
      paste0(collapse = "\n") |>
      textConnection() |>
      read.csv(stringsAsFactors = FALSE) |> # Read as data.frame without factors
      dplyr::rename_with(~ gsub("^X\\.", "", .x)) |> # Remove the "X." prefix
      dplyr::rename_with(~ sub("\\..*", "", .x)) |> # Remove everything after the first `.`
      janitor::clean_names() |> # Clean column names
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ stringr::str_replace_all(.x, "'", "") |>
          stringr::str_trim()),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        stream_order = as.numeric(stream_order)
      ) |>
      readr::type_convert()
  }

  suppressMessages({
    suppressWarnings({
      study <- list(
        input_files[grepl("cabin_study_data_maritimes.csv", input_files)] |>
          import_cabin() |>
          dplyr::mutate(bassin = "maritimes") |>
          dplyr::relocate(bassin),
        input_files[grepl("cabin_study_data_st_lawrence.csv", input_files)] |>
          import_cabin() |>
          dplyr::mutate(bassin = "st_lawrence") |>
          dplyr::relocate(bassin)
      ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
          project_id = "cabin_aquatic_biomonitoring",
          project_name = "Base de données du Réseau canadien de biosurveillance aquatique (RCBA)",
          event_id = glue::glue("{bassin}-{site}-{site_visit_id}"),
          event_date = as.Date(julian_day - 1, origin = paste0(year, "-01-01"))
        )
    })
  })



  # Export
  vroom::vroom_write(benthic, file.path(output_path, "cabin_benthic_data.csv"), delim = ",")
  vroom::vroom_write(habitat, file.path(output_path, "cabin_habitat_data.csv"), delim = ",")
  vroom::vroom_write(study, file.path(output_path, "cabin_study_data.csv"), delim = ",")
}
