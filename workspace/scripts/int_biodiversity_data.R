int_biodiversity_data <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/biodiversity_data-1.0.0"
  # # dir.create(output_path)
  # input_files <- file.path(
  #   "workspace/data/harvested",
  #   c(
  #     "biodiversite_piei-1.0.0/processed/events_piei.csv",
  #     "biodiversite_piei-1.0.0/processed/occurrences_piei.csv",
  #     "biome_recharge_io_herbier_benthos-1.0.0/processed/biome_recharge_io_herbier_benthos_logbooks.csv",
  #     "biome_recharge_lagrave-1.0.0/processed/biome_recharge_lagrave_biodiversity.csv",
  #     "biome_recharge_lagrave-1.0.0/processed/biome_recharge_lagrave_logbooks.csv",
  #     "biome_recharge_maria-1.0.0/processed/biome_recharge_maria_logbooks.csv",
  #     "biome_recharge_pointe-aux-loups-1.0.0/processed/biome_recharge_pointe-aux-loups_biodiversity.csv",
  #     "biome_recharge_pointe-aux-loups-1.0.0/processed/biome_recharge_pointe-aux-loups_logbooks.csv",
  #     "biome_recharge_pointe-aux-outardes-1.0.0/processed/biome_recharge_pointe-aux-outardes_biodiversity.csv",
  #     "biome_recharge_pointe-aux-outardes-1.0.0/processed/biome_recharge_pointe-aux-outardes_logbooks.csv",
  #     "biome_recharge_sainte-flavie-1.0.0/processed/biome_recharge_sainte-flavie_logbooks.csv",
  #     "biome_recharge_sainte-luce-1.0.0/processed/biome_recharge_sainte-luce_logbooks.csv",
  #     "cabin_aquatic_biomonitoring-1.0.0/processed/cabin_benthic_data.csv",
  #     "cabin_aquatic_biomonitoring-1.0.0/processed/cabin_habitat_data.csv",
  #     "cabin_aquatic_biomonitoring-1.0.0/processed/cabin_study_data.csv",
  #     "inventaire_batture_alouettes-1.0.0/processed/kelp_events.csv",
  #     "inventaire_batture_alouettes-1.0.0/processed/kelp_occurrences.csv",
  #     "inventaire_macroalgues_macroinvertebres-1.0.0/processed/abiotic.csv",
  #     "inventaire_macroalgues_macroinvertebres-1.0.0/processed/events.csv",
  #     "inventaire_macroalgues_macroinvertebres-1.0.0/processed/occurrences.csv",
  #     "inventaire_pointe_john-1.0.0/processed/events_pointe_john.csv",
  #     "inventaire_pointe_john-1.0.0/processed/occurrences_pointe_john.csv",
  #     "inventaire_pointe_john-1.0.0/processed/physico_chem_pointe_john.csv",
  #     "inventaire_pointe_john-1.0.0/processed/species_recovery_pointe_john.csv",
  #     "invertebres_vegetation_saint_laurent-1.0.0/processed/dictionnaire_donnees.csv",
  #     "invertebres_vegetation_saint_laurent-1.0.0/processed/evenements_cpd.csv",
  #     "invertebres_vegetation_saint_laurent-1.0.0/processed/occurrences_cpd.csv",
  #     "yanick_gendreau-1.0.0/processed/yanick_gendreau_intertidal_subtidal.csv",
  #     "yanick_gendreau-1.0.0/processed/yanick_gendreau_intertidal.csv",
  #     "zip_inventaire_marais_baie_mille_vaches-1.0.0/processed/marais_bmv_abiotic.csv",
  #     "zip_inventaire_marais_baie_mille_vaches-1.0.0/processed/marais_bmv_abundance.csv",
  #     "zip_inventaire_marais_baie_mille_vaches-1.0.0/processed/marais_bmv_events.csv",
  #     "zip_inventaire_marais_baie_mille_vaches-1.0.0/processed/marais_bmv_occurrences.csv",
  #     "zip_inventaire_marais_bergeronnes-1.0.0/processed/marais_bergeronnes_abiotic.csv",
  #     "zip_inventaire_marais_bergeronnes-1.0.0/processed/marais_bergeronnes_abundance.csv",
  #     "zip_inventaire_marais_bergeronnes-1.0.0/processed/marais_bergeronnes_events.csv",
  #     "zip_inventaire_marais_bergeronnes-1.0.0/processed/marais_bergeronnes_occurrences.csv",
  #     "zip_inventaire_marais_hickey-1.0.0/processed/marais_hick_abiotic.csv",
  #     "zip_inventaire_marais_hickey-1.0.0/processed/marais_hick_abundance.csv",
  #     "zip_inventaire_marais_hickey-1.0.0/processed/marais_hick_events.csv",
  #     "zip_inventaire_marais_hickey-1.0.0/processed/marais_hick_occurrences.csv",
  #     "zip_inventaire_marais_pointe_aux_outardes-1.0.0/processed/marais_pao_abiotic.csv",
  #     "zip_inventaire_marais_pointe_aux_outardes-1.0.0/processed/marais_pao_abundance.csv",
  #     "zip_inventaire_marais_pointe_aux_outardes-1.0.0/processed/marais_pao_events.csv",
  #     "zip_inventaire_marais_pointe_aux_outardes-1.0.0/processed/marais_pao_occurrences.csv",
  #     "zip_inventaire_marais_pointe_fortin-1.0.0/processed/marais_pointe_fortin_abiotic.csv",
  #     "zip_inventaire_marais_pointe_fortin-1.0.0/processed/marais_pointe_fortin_abundance.csv",
  #     "zip_inventaire_marais_pointe_fortin-1.0.0/processed/marais_pointe_fortin_events.csv",
  #     "zip_inventaire_marais_pointe_fortin-1.0.0/processed/marais_pointe_fortin_occurrences.csv",
  #     "zip_inventaire_marais_portneuf_sur_mer-1.0.0/processed/marais_portneuf_sur_mer_abiotic.csv",
  #     "zip_inventaire_marais_portneuf_sur_mer-1.0.0/processed/marais_portneuf_sur_mer_abundance.csv",
  #     "zip_inventaire_marais_portneuf_sur_mer-1.0.0/processed/marais_portneuf_sur_mer_events.csv",
  #     "zip_inventaire_marais_portneuf_sur_mer-1.0.0/processed/marais_portneuf_sur_mer_occurrences.csv"
  #   )
  # )
  input_files <- unlist(input_files)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data sources

  # -------------------------------------------------------
  # Data tables
  dat <- list()

  # Iterate over input files and populate the dat list
  for (file in input_files) {
    # Extract database name
    db_name <- stringr::str_extract(file, "(?<=harvested/|analyzed/)[^/]+")
    db_name <- stringr::str_remove(db_name, "-\\d+\\.\\d+\\.\\d+$") # Remove version

    # Extract table name
    table_name <- stringr::str_remove(basename(file), "\\.csv$") # Remove file extension

    # Load data using vroom
    suppressWarnings({
      df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
    })

    # Store in nested list
    if (!db_name %in% names(dat)) {
      dat[[db_name]] <- list()
    }
    dat[[db_name]][[table_name]] <- df
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tables

  # ----------------------------------------------------------------------
  # Yanick Gendreau data, which has a different format than the other tables
  gendreau <- input_files[grepl("yanick_gendreau_intertidal.csv", input_files)] |>
    vroom::vroom(progress = FALSE, show_col_types = FALSE)


  # ----------------------------------------------------------------------
  # Events tables
  event_tables <- list()

  # Define patterns to identify event-related tables
  patterns <- c("events", "logbooks", "study", "evenements")

  # Column mapping for standardization
  column_map <- list(
    project_id = c(
      "project_id", "dataset_id", "jeu_donnee_id"
    ),
    project_name = c(
      "dataset_name", "nom_jeu_donnee", "study"
    ),
    parent_event_id = c(
      "parent_event_id"
    ),
    site_id = c(
      "site", "site_id", "location_id"
    ),
    site_name = c(
      "site_name", "locality", "location"
    ),
    event_id = c(
      "event_id", "activite_id"
    ),
    event_date = c(
      "event_date", "date", "date_activite"
    ),
    # event_date_start = c(
    #   "date_pose"
    # ),
    # event_date_end = c(
    #   "date_levee"
    # ),
    longitude = c(
      "decimal_longitude", "longitude", "longitude_decimal"
    ),
    latitude = c(
      "decimal_latitude", "latitude", "latitude_decimal"
    ),
    elevation = c
    ("elevation", "maximum_elevation_in_meters"),
    sampling_protocol = c(
      "sampling_protocol", "methode_echantillonnage", "type_engin"
    ),
    sampling_effort = c(
      "sampling_effort", "effort_echantillonnage"
    ),
    sample_size_value = c(
      "sample_size_value", "valeur_taille_echantillon", "temps_total_de_peche_13", "temps_total_peche_heure"
    ),
    sample_size_unit = c(
      "sample_size_unit", "unite_taille_echantillon", "unite"
    ),
    geodetic_datum = c(
      "geodetic_datum", "empreinte_geographique_srs"
    ),
    remarks = c(
      "event_remarks", "remarques_terrain", "commentaires"
    ),
    depth = c(
      "maximum_depth_in_meters", "depth_value", "profondeur_maximum_en_metres",
      "profondeur_minimum_en_metres", "profondeur_verbatim", "profondeur_m"
    ) # ,
    # vegetation_cover = c(
    #   "couverture_vegetation"
    # ),
    # sand_cover = c(
    #   "couverture_sable"
    # ),
    # substrate = c(
    #   "substrat"
    # ),
    # habitat = c(
    #   "habitat"
    # )
  )

  # Iterate over input files and load data
  for (file in input_files) {
    # Extract database name and table name
    db_name <- stringr::str_extract(file, "(?<=harvested/|analyzed/)[^/]+")
    db_name <- stringr::str_remove(db_name, "-\\d+\\.\\d+\\.\\d+$") # Remove version
    table_name <- stringr::str_remove(basename(file), "\\.csv$") # Remove file extension

    # Load data using vroom
    suppressWarnings({
      df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
    })

    # Check if the table name matches any of the event-related patterns
    if (any(stringr::str_detect(table_name, patterns))) {
      # Standardize column names
      colnames(df) <- tolower(colnames(df))

      # Rename columns based on the mapping
      for (standard_col in names(column_map)) {
        matching_cols <- column_map[[standard_col]]
        existing_col <- intersect(matching_cols, colnames(df))

        if (length(existing_col) > 0) {
          df <- df |> dplyr::rename(!!standard_col := all_of(existing_col[1]))
        }
      }

      # Ensure required columns are present
      required_columns <- c("project_id", "event_id", "event_date", "longitude", "latitude")
      validate_columns(df, required_columns)

      # Select only mapped columns and format dates
      df <- df |>
        dplyr::select(dplyr::all_of(intersect(names(column_map), colnames(df)))) |>
        dplyr::mutate(event_date = as.character(event_date)) |>
        dplyr::rowwise() |>
        dplyr::mutate(date_processed = list(process_event_dates(event_date))) |>
        tidyr::unnest(date_processed) |>
        dplyr::select(-event_date)


      # Append to list of event tables
      event_tables[[paste(db_name, table_name, sep = "_")]] <- df
    }
  }

  # Combine all event data into a single data frame
  event_tables <- dplyr::bind_rows(event_tables)


  # -------------------------------------------------------
  # Biodiversity tables







  # ----------------------------------------------------------------------
  # Abiotic tables
  abiotic_tables <- list()

  # Define patterns to identify event-related tables
  patterns <- c("abiotic", "habitat_data", "physico_chem")

  # Column mapping for standardization
  column_map <- list(
    event_id = c(
      "event_id", "activite_id"
    ),
    measurement_category = c(
      "measurement_category", "type"
    ),
    measurement_type = c(
      "measurement_type", "variable"
    ),
    measurement_type_description = c(
      "measurement_type_description", "variable_description"
    ),
    measurement_value = c(
      "measurement_value", "value"
    ),
    measurement_unit = c(
      "measurement_unit", "unit"
    ),
    measurement_method = c(
      "measurement_method", "protocol"
    ),
    measurement_remarks = c(
      "measurement_remarks", "note"
    )
  )

  # Iterate over input files and load data
  for (file in input_files) {
    # Extract database name and table name
    db_name <- stringr::str_extract(file, "(?<=harvested/|analyzed/)[^/]+")
    db_name <- stringr::str_remove(db_name, "-\\d+\\.\\d+\\.\\d+$") # Remove version
    table_name <- stringr::str_remove(basename(file), "\\.csv$") # Remove file extension

    # Load data using vroom
    suppressWarnings({
      df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
    })

    # Check if the table name matches any of the event-related patterns
    if (any(stringr::str_detect(table_name, patterns))) {
      # Standardize column names
      colnames(df) <- tolower(colnames(df))

      # Rename columns based on the mapping
      for (standard_col in names(column_map)) {
        matching_cols <- column_map[[standard_col]]
        existing_col <- intersect(matching_cols, colnames(df))

        if (length(existing_col) > 0) {
          df <- df |> dplyr::rename(!!standard_col := all_of(existing_col[1]))
        }
      }

      # Ensure required columns are present
      required_columns <- c("event_id")
      validate_columns(df, required_columns)

      # Select only mapped columns and format dates
      df <- df |>
        dplyr::select(dplyr::all_of(intersect(names(column_map), colnames(df)))) |>
        dplyr::mutate(measurement_value = as.character(measurement_value))

      # Append to list of event tables
      abiotic_tables[[paste(db_name, table_name, sep = "_")]] <- df
    }
  }

  # Combine all event data into a single data frame
  abiotic_tables <- dplyr::bind_rows(abiotic_tables)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Relational database tables
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Events table
  events <- dplyr::bind_rows(
    event_tables,
    gendreau |>
      dplyr::select(project_id, event_id, event_date_start = event_date, site_name, longitude, latitude) |>
      dplyr::mutate(event_date_end = event_date_start)
  ) |>
    dplyr::select(-project_name) |>
    dplyr::distinct()

  # Abiotic table
  abiotic <- abiotic_tables |>
    dplyr::distinct()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Relational database
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initiate sqlite
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(output_path, "biodiversity_data.sqlite"))

  # Add tables
  DBI::dbWriteTable(con, "events", events, overwrite = TRUE)
  DBI::dbWriteTable(con, "abiotic", abiotic, overwrite = TRUE)

  # BD Schema
  dm::dm_from_con(con) |>
    dm::dm_add_pk(table = "events", "event_id") |>
    dm::dm_add_pk(table = "abiotic", "event_id") |>
    dm::dm_add_fk(table = "abiotic", "event_id", "events") |>
    dm::dm_draw(view_type = "all", rankdir = "BT", column_types = TRUE) |>
    DiagrammeRsvg::export_svg() |>
    write(file.path(output_path, "biodiversity_data.svg"))

  DBI::dbDisconnect(con)
}




process_event_dates <- function(event_date) {
  parse_functions <- list(
    lubridate::ymd_hms,
    lubridate::ymd_hm,
    lubridate::ymd
  )

  parse_date <- function(date_str) {
    parsed_dates <- purrr::map(parse_functions, ~ .x(date_str, quiet = TRUE))
    dplyr::coalesce(!!!parsed_dates)
  }

  if (is.na(event_date)) {
    start_date <- end_date <- NA
  } else if (stringr::str_detect(event_date, "/")) {
    dates <- stringr::str_split(event_date, "/")[[1]]
    start_date <- parse_date(dates[1])
    end_date <- parse_date(dates[2])
  } else {
    start_date <- end_date <- parse_date(event_date)
  }

  return(tibble::tibble(event_date_start = start_date, event_date_end = end_date))
}

# Function to check for missing columns
validate_columns <- function(df, required_columns) {
  missing_cols <- setdiff(required_columns, colnames(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("The following required columns are missing: {paste(missing_cols, collapse = ', ')} from table {table_name} in dataset {db_name}.")
  }
}
