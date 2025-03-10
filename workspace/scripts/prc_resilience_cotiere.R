prc_resilience_cotiere <- function(input_files, output_path) {
  # output_path <- "workspace/data/harvested/resilience_cotiere-1.0.0/processed"
  # input_path <- "workspace/data/harvested/resilience_cotiere-1.0.0/raw"
  # input_files <- file.path(
  #   input_path,
  #   c(
  #     "CartographieEcosystemes_QcMaritime_2021.gdb.zip",
  #     "Cotes_QcMaritime_2022_03_03.gdb.zip"
  #    )
  # )
  input_files <- unlist(input_files)

  # Decompress
  tmp <- file.path(output_path, "tmp")
  lapply(input_files, function(x) archive::archive_extract(x, tmp))

  # Load and process
  suppressWarnings({
    eco <- sf::st_read(
      file.path(tmp, c("CartographieEcosystemes_QcMaritime_2021.gdb")),
      layer = "CartographieEcosystemes_QcMaritime_2021",
      quiet = TRUE
    ) |>
      dplyr::filter(sf::st_geometry_type(Shape) == "MULTIPOLYGON") |>
      sf::st_make_valid() |>
      janitor::clean_names() |>
      dplyr::mutate(
        ecosystem_id = paste0(
          "eco_",
          stringr::str_pad(
            as.integer(as.factor(eco_simp)),
            width = 2,
            pad = "0"
          )
        )
      ) |>
      dplyr::select(ecosystem_id, ecosystem = eco_simp)
  })

  suppressWarnings({
    cote <- sf::st_read(
      file.path(tmp, c("Cotes_QcMaritime_2022_03_03.gdb")),
      layer = "QcMaritime_QcLambert_2022_03_03",
      quiet = TRUE
    ) |>
      dplyr::filter(sf::st_geometry_type(Shape) == "MULTILINESTRING") |>
      sf::st_make_valid() |>
      janitor::clean_names()
  })

  # Export
  sf::st_write(eco, file.path(output_path, "resilience_cotiere_ecosystemes.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(cote, file.path(output_path, "resilience_cotiere_cotes.gpkg"), delete_dsn = TRUE, quiet = TRUE)

  # Clean up temporary files
  fs::dir_delete(tmp)
}
