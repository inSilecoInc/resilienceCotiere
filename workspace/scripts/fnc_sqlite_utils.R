# Wrapper to connect to relational database
connect_sqlite <- function(input_files) {
  DBI::dbConnect(
    RSQLite::SQLite(),
    input_files[grepl(".sqlite", input_files)]
  )
}

get_biodiversity <- function(input_files, measurement, unit, wide = TRUE) {
  # Connect to relational database
  con <- connect_sqlite(input_files)
  withr::defer(DBI::dbDisconnect(con))

  # Reference tables
  biodiversity <- dplyr::tbl(con, "biodiversity")

  biodiversity <- biodiversity |>
    dplyr::filter(
      measurement_type == measurement,
      measurement_unit == unit | is.na(measurement_unit)
    ) |>
    dplyr::group_by(event_id, species_id) |>
    dplyr::summarise(total = sum(measurement_value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(total = dplyr::if_else(is.na(total), 0, total)) |>
    dplyr::collect() |>
    tidyr::drop_na()

  if (wide) {
    biodiversity <- biodiversity |>
      tidyr::pivot_wider(
        names_from = species_id,
        values_from = total,
        values_fill = list(total = 0)
      )
  }

  biodiversity
}

get_taxonomy <- function(input_files) {
  # Connexion à la base de données SQLite
  con <- connect_sqlite(input_files)
  withr::defer(DBI::dbDisconnect(con))

  # Référencer la table biodiversité et taxonomie
  tx <- dplyr::tbl(con, "taxonomy") |>
    dplyr::collect()

  return(tx)
}
