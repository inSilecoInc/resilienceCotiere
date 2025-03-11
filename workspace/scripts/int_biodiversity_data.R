int_biodiversity_data <- function(input_files, output_path) {
  # output_path <- "workspace/data/analyzed/biodiversity_data-1.0.0"
  # # dir.create(output_path)
  # input_files <- file.path(
  #   "workspace/data/harvested",
  #   c(
  #     "biodiversite_piei-1.0.0/processed/events_piei.csv",
  #     "biodiversite_piei-1.0.0/processed/occurrences_piei.csv",
  #     "biome_recharge_io_herbier_benthos-1.0.0/processed/biome_recharge_io_herbier_benthos_logbooks.csv",
  #     "biome_recharge_io_herbier_benthos-1.0.0/processed/biome_recharge_io_herbier_benthos_biodiversity.csv",
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
  #     "zip_inventaire_marais_portneuf_sur_mer-1.0.0/processed/marais_portneuf_sur_mer_occurrences.csv",
  #     "resilience_cotiere-1.0.0/processed/resilience_cotiere_ecosystemes.gpkg"
  #   )
  # )
  input_files <- unlist(input_files)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tables

  # ----------------------------------------------------------------------
  # Events tables
  make_events_tables <- function() {
    event_tables <- list()

    # Define patterns to identify event-related tables
    patterns <- c("events", "logbooks", "study", "evenements")

    # Column mapping for standardization
    column_map <- list(
      project_id = c(
        "project_id", "dataset_id", "jeu_donnee_id"
      ),
      project_name = c(
        "project_name", "dataset_name", "nom_jeu_donnee"
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

      # Check if the table name matches any of the event-related patterns
      if (any(stringr::str_detect(table_name, patterns))) {
        # Load data using vroom
        suppressWarnings({
          df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
        })
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

    # ----------------------------------------------------------------------
    # Yanick Gendreau data, which has a different format than the other tables
    gendreau <- input_files[grepl("yanick_gendreau_intertidal.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE)

    event_tables <- dplyr::bind_rows(
      event_tables,
      gendreau |>
        dplyr::select(project_id, project_name, event_id, event_date_start = event_date, site_name, longitude, latitude) |>
        dplyr::mutate(event_date_end = event_date_start)
    )

    # Add spatial filtering to remove erroneous data or data falling outside of the area of interest for this project
    event_tables <- event_tables |>
      dplyr::filter(
        longitude >= -74 & longitude <= -57 &
          latitude >= 45 & latitude <= 51
      )

    return(event_tables)
  }




  # -------------------------------------------------------
  # Biodiversity tables
  make_biodiversity_tables <- function() {
    biodiversity_tables <- list()

    # -------------------
    # Biodiversity tables
    biodiversity_tables$biome <- list()
    for (file in input_files) {
      # Extract database name and table name
      db_name <- stringr::str_extract(file, "(?<=harvested/|analyzed/)[^/]+")
      db_name <- stringr::str_remove(db_name, "-\\d+\\.\\d+\\.\\d+$") # Remove version
      table_name <- stringr::str_remove(basename(file), "\\.csv$") # Remove file extension

      # Load data using vroom
      if (any(stringr::str_detect(table_name, "biodiversity"))) {
        suppressWarnings({
          df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
        })

        # Append to list of event tables
        biodiversity_tables$biome[[paste(db_name, table_name, sep = "_")]] <- df
      }
    }
    biodiversity_tables$biome <- dplyr::bind_rows(biodiversity_tables$biome) |>
      dplyr::select(
        event_id,
        aphiaid = valid_aphiaid, scientific_name = valid_name,
        taxon_rank = rank, kingdom, phylum, class, order, family, genus, species,
        abundance_m2, biomass_m2, occurrence
      ) |>
      tidyr::pivot_longer(
        cols = c(abundance_m2, biomass_m2, occurrence), # Columns to reshape
        names_to = "measurement_type",
        values_to = "measurement_value"
      ) |>
      dplyr::mutate(
        measurement_type = dplyr::recode(
          measurement_type,
          abundance_m2 = "abundance",
          biomass_m2 = "biomass",
          occurrence = "occurrence"
        ),
        measurement_unit = dplyr::case_when(
          measurement_type == "abundance" ~ "n/m2",
          measurement_type == "biomass" ~ "g/m2",
          measurement_type == "occurrence" ~ NA_character_
        )
      ) |>
      dplyr::rename(scientific_name_id = aphiaid) |>
      dplyr::mutate(scientific_name_id_db = "worms")

    # ------------------------------
    # Abundance - occurrences tables
    comb <- data.frame(
      abundance = c(
        "marais_bmv_abundance.csv",
        "marais_bergeronnes_abundance.csv",
        "marais_hick_abundance.csv",
        "marais_pao_abundance.csv",
        "marais_pointe_fortin_abundance.csv",
        "marais_portneuf_sur_mer_abundance.csv"
      ),
      occurrence = c(
        "marais_bmv_occurrences.csv",
        "marais_bergeronnes_occurrences.csv",
        "marais_hick_occurrences.csv",
        "marais_pao_occurrences.csv",
        "marais_pointe_fortin_occurrences.csv",
        "marais_portneuf_sur_mer_occurrences.csv"
      )
    )

    biodiversity_tables$zip <- list()
    for (i in seq_len(nrow(comb))) {
      # Load data using vroom
      suppressWarnings({
        occurrence <- input_files[grepl(comb$occurrence[i], input_files)] |>
          vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",")
        abundance <- input_files[grepl(comb$abundance[i], input_files)] |>
          vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",")
      })

      df <- dplyr::left_join(
        occurrence,
        abundance |>
          dplyr::mutate(
            measurement_type = stringr::str_replace_all(
              measurement_type,
              c(
                "Individual count" = "Nombre individu",
                "Nombre d'individu" = "Nombre individu",
                "Nombre de plant" = "Nombre individu"
              )
            )
          ) |>
          dplyr::filter(measurement_type %in% c("Poids", "Nombre individu", "Taux de recouvrement")) |>
          dplyr::group_by(event_id, occurrence_id, measurement_type) |>
          dplyr::group_split() |>
          lapply(function(x) {
            if (x$measurement_type[1] == "Taux de recouvrement") {
              data.frame(
                event_id = x$event_id[1],
                occurrence_id = x$occurrence_id[1],
                measurement_type = x$measurement_type[1],
                measurement_value = x$measurement_value[1],
                measurement_unit = x$measurement_unit[1],
                measurement_n = nrow(x)
              )
            } else {
              data.frame(
                event_id = x$event_id[1],
                occurrence_id = x$occurrence_id[1],
                measurement_type = x$measurement_type[1],
                measurement_value = x$measurement_value |>
                  stringr::str_replace_all(
                    c(
                      "\\+" = "",
                      "<" = "",
                      ">" = "",
                      "5-25" = "15",
                      "25-50" = "38",
                      "50-75" = "63"
                    )
                  ) |>
                  as.numeric() |>
                  sum(na.rm = TRUE) |>
                  as.character(),
                measurement_unit = x$measurement_unit[1],
                measurement_n = nrow(x)
              )
            }
          }) |>
          dplyr::bind_rows() |>
          dplyr::select(
            event_id, occurrence_id, measurement_type, measurement_value,
            measurement_unit, measurement_n
          ) |>
          dplyr::mutate(measurement_type = stringr::str_replace_all(
            measurement_type,
            c(
              "Poids" = "biomass",
              "Nombre individu" = "abundance",
              "Taux de recouvrement" = "recouvrement"
            )
          )),
        by = c("event_id", "occurrence_id")
      ) |>
        dplyr::filter(!is.na(measurement_type))

      # Check if 'organism_quantity' exists before applying mutate
      suppressWarnings({
        if ("organism_quantity" %in% names(df)) {
          df <- df |>
            dplyr::mutate(
              measurement_value = dplyr::if_else(
                !is.na(measurement_type) & measurement_type == "Poids",
                as.character((as.numeric(measurement_value) * as.numeric(organism_quantity)) / measurement_n),
                measurement_value
              )
            )
        }
      })

      # The previous part covers whatever data is available in the abundance dataset
      # Now getting the occurrence data from the occurrence dataset
      occ <- occurrence |>
        dplyr::mutate(
          measurement_type = "occurrence",
          measurement_value = dplyr::case_when(
            tolower(occurrence_status) == "present" ~ "1",
            tolower(occurrence_status) == "absent" ~ "0",
            .default = NA
          ),
          measurement_unit = NA
        )

      # Append to list of event tables
      biodiversity_tables$zip[[i]] <- dplyr::bind_rows(occ, df)
    }

    biodiversity_tables$zip <- dplyr::bind_rows(biodiversity_tables$zip) |>
      dplyr::select(
        event_id,
        scientific_name = accepted_name_usage, scientific_name_id = accepted_name_usage_id,
        gbif_url = taxon_id, taxon_rank, measurement_type, measurement_value, measurement_unit
      ) |>
      dplyr::arrange(event_id) |>
      dplyr::mutate(
        scientific_name_id_db = dplyr::case_when(
          stringr::str_detect(scientific_name_id, "marinespecies.org") ~ "worms",
          stringr::str_detect(scientific_name_id, "itis.gov") ~ "itis",
          stringr::str_detect(scientific_name_id, "indexfungorum.org") ~ "fungorum",
          stringr::str_detect(scientific_name_id, "ipni.org") ~ "ipni",
          .default = NA
        ),
        scientific_name_id = stringr::str_replace(scientific_name_id, ".*:", "")
      )

    # ----------------------
    # Inventaire Pointe-John
    suppressWarnings({
      occurrence <- input_files[grepl("occurrences_pointe_john.csv", input_files)] |>
        vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",")
      abundance <- input_files[grepl("species_recovery_pointe_john.csv", input_files)] |>
        vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",")
    })

    # Abundance data
    df <- dplyr::left_join(
      occurrence,
      abundance |>
        dplyr::group_by(event_id, occurrence_id, measurement_type) |>
        dplyr::group_split() |>
        lapply(function(x) {
          data.frame(
            event_id = x$event_id[1],
            occurrence_id = x$occurrence_id[1],
            measurement_type = x$measurement_type[1],
            measurement_value = x$measurement_value |>
              as.numeric() |>
              sum(na.rm = TRUE) |>
              as.character(),
            measurement_unit = x$measurement_unit[1],
            measurement_n = nrow(x)
          )
        }) |>
        dplyr::bind_rows() |>
        dplyr::select(event_id, occurrence_id, measurement_type, measurement_value, measurement_unit),
      by = c("event_id", "occurrence_id")
    ) |>
      dplyr::filter(!is.na(scientific_name)) |>
      dplyr::filter(!is.na(measurement_type)) |>
      dplyr::mutate(measurement_type = stringr::str_replace_all(
        measurement_type,
        c(
          "Nombre d'individus" = "abundance",
          "Recouvrement" = "recouvrement"
        )
      ))

    # The previous part covers whatever data is available in the abundance dataset
    # Now getting the occurrence data from the occurrence dataset
    occ <- occurrence |>
      dplyr::mutate(
        measurement_type = "occurrence",
        measurement_value = dplyr::case_when(
          tolower(occurrence_status) == "present" ~ "1",
          tolower(occurrence_status) == "absent" ~ "0",
          .default = NA
        ),
        measurement_unit = NA
      )

    # Bind together
    biodiversity_tables$inventaire_pointe_john <- dplyr::bind_rows(df, occ) |>
      dplyr::select(
        event_id, scientific_name, scientific_name_id, taxon_rank,
        gbif_url = taxon_id, measurement_type, measurement_value,
        measurement_unit
      ) |>
      dplyr::mutate(
        scientific_name_id_db = dplyr::case_when(
          stringr::str_detect(scientific_name_id, "marinespecies.org") ~ "worms",
          stringr::str_detect(scientific_name_id, "itis.gov") ~ "itis",
          stringr::str_detect(scientific_name_id, "indexfungorum.org") ~ "fungorum",
          stringr::str_detect(scientific_name_id, "ipni.org") ~ "ipni",
          .default = NA
        ),
        scientific_name_id = stringr::str_replace(scientific_name_id, ".*:", "")
      )

    # ----------------------
    # piei: anglais, darwin
    # iba: anglais, darwin
    # imm: anglais, darwin
    biodiversity_tables$inventaires <- c(
      "biodiversite_piei-1.0.0/processed/occurrences_piei.csv",
      "inventaire_batture_alouettes-1.0.0/processed/kelp_occurrences.csv",
      "inventaire_macroalgues_macroinvertebres-1.0.0/processed/occurrences.csv"
    ) |>
      lapply(function(x) {
        input_files[grepl(x, input_files)] |>
          vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",") |>
          dplyr::mutate(taxon_id = as.character(taxon_id))
      }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        measurement_type = "occurrence",
        measurement_value = dplyr::case_when(
          tolower(occurrence_status) == "present" ~ "1",
          tolower(occurrence_status) == "presence" ~ "1",
          tolower(occurrence_status) == "absent" ~ "0",
          tolower(occurrence_status) == "absence" ~ "0",
          .default = NA
        ),
        measurement_unit = NA
      ) |>
      dplyr::select(
        event_id, scientific_name, scientific_name_id,
        taxon_rank, kingdom, phylum, class, order, family, genus,
        gbif_url = taxon_id, measurement_type, measurement_value,
        measurement_unit
      ) |>
      dplyr::mutate(
        species = scientific_name,
        gbif_url = dplyr::if_else(
          stringr::str_detect(gbif_url, "^[0-9]+$"),
          glue::glue("https://www.gbif.org/species/{gbif_url}"),
          gbif_url
        ),
        gbif_url = dplyr::if_else(gbif_url == "notfound", NA, gbif_url),
        scientific_name_id_db = dplyr::case_when(
          stringr::str_detect(scientific_name_id, "marinespecies.org") ~ "worms",
          stringr::str_detect(scientific_name_id, "itis.gov") ~ "itis",
          stringr::str_detect(scientific_name_id, "indexfungorum.org") ~ "fungorum",
          stringr::str_detect(scientific_name_id, "ipni.org") ~ "ipni",
          .default = NA
        ),
        scientific_name_id = stringr::str_replace(scientific_name_id, ".*:", "")
      )


    # ----------------------
    # Cabin
    biodiversity_tables$cabin <- input_files[grepl("cabin_benthic_data.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",") |>
      dplyr::mutate(scientific_name = dplyr::case_when(
        !is.na(species) ~ species,
        !is.na(genus) ~ genus,
        !is.na(family) ~ family,
        !is.na(order) ~ order,
        !is.na(class) ~ class,
        !is.na(phylum) ~ phylum,
        .default = NA
      )) |>
      dplyr::select(
        event_id, itis_tsn, scientific_name,
        phylum, class, order, family, genus, species,
        measurement_value = count
      ) |>
      dplyr::mutate(measurement_type = "abundance", measurement_unit = NA)

    # Add occurrence measurements
    biodiversity_tables$cabin <- dplyr::bind_rows(
      biodiversity_tables$cabin,
      biodiversity_tables$cabin |>
        dplyr::mutate(
          measurement_type = "occurrence",
          measurement_value = dplyr::if_else(measurement_value > 0, 1, 0),
          measurement_unit = "n"
        )
    ) |>
      dplyr::arrange(event_id, scientific_name) |>
      dplyr::mutate(measurement_value = as.character(measurement_value)) |>
      dplyr::rename(scientific_name_id = itis_tsn) |>
      dplyr::mutate(scientific_name_id_db = "itis")


    # ----------------------
    # Invertebres Saint-Laurent
    biodiversity_tables$ivsl <- input_files[grepl("occurrences_cpd.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE, delim = ",") |>
      dplyr::select(
        event_id = id, statut_occurrence, nom_scientifique,
        kingdom = regne, phylum = embranchement, class = classe,
        order = ordre, family = famille, genus = genre, species = espece,
        scientific_name_id = nom_scientifique_id
      ) |>
      dplyr::mutate(
        measurement_type = "occurrence",
        measurement_value = dplyr::case_when(
          tolower(statut_occurrence) == "present" ~ "1",
          tolower(statut_occurrence) == "presence" ~ "1",
          tolower(statut_occurrence) == "absent" ~ "0",
          tolower(statut_occurrence) == "absence" ~ "0",
          .default = NA
        ),
        measurement_unit = NA
      ) |>
      dplyr::select(-statut_occurrence) |>
      dplyr::mutate(
        scientific_name_id_db = dplyr::case_when(
          stringr::str_detect(scientific_name_id, "marinespecies.org") ~ "worms",
          stringr::str_detect(scientific_name_id, "itis.gov") ~ "itis",
          stringr::str_detect(scientific_name_id, "indexfungorum.org") ~ "fungorum",
          stringr::str_detect(scientific_name_id, "ipni.org") ~ "ipni",
          .default = NA
        ),
        scientific_name_id = stringr::str_replace(scientific_name_id, ".*:", "")
      )

    # ----------------------
    # Yanick Gendreau
    # Create a function to find matching species
    match_species <- function(species_name, df) {
      matched_row <- df |>
        dplyr::filter(stringr::str_detect(species_name, nom_original)) |>
        dplyr::slice(1)
      return(
        matched_row |>
          dplyr::select(aphia_id_accepted, nom_original)
      )
    }

    df <- input_files[grepl("yanick_gendreau_intertidal_subtidal.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      dplyr::select(aphia_id_accepted, nom_original) |>
      dplyr::distinct()

    biodiversity_tables$gendreau <- input_files[grepl("yanick_gendreau_intertidal.csv", input_files)] |>
      vroom::vroom(progress = FALSE, show_col_types = FALSE) |>
      dplyr::mutate(species = stringr::str_replace_all(species, "\\s*\\(.*?\\)", "")) |>
      dplyr::rowwise() |>
      dplyr::mutate(match = list(match_species(species, df))) |>
      tidyr::unnest_wider(match) |>
      dplyr::select(-species) |>
      dplyr::rename(species = nom_original) |>
      dplyr::select(
        event_id, recouvrement_percent, biomasse_g, abondance, presence,
        scientific_name_id = aphia_id_accepted, species
      ) |>
      dplyr::mutate(
        scientific_name_id_db = "worms",
        abondance = as.character(abondance),
        biomasse_g = as.character(biomasse_g),
        presence = as.character(presence),
        recouvrement_percent = as.character(recouvrement_percent)
      ) |>
      tidyr::pivot_longer(
        cols = c(abondance, biomasse_g, presence, recouvrement_percent), # Columns to reshape
        names_to = "measurement_type",
        values_to = "measurement_value"
      ) |>
      dplyr::mutate(
        measurement_type = dplyr::recode(
          measurement_type,
          abondance = "abundance",
          biomasse_g = "biomass",
          presence = "occurrence",
          recouvrement_percent = "recouvrement"
        ),
        measurement_unit = dplyr::case_when(
          measurement_type == "abundance" ~ "n",
          measurement_type == "biomass" ~ "g",
          measurement_type == "occurrence" ~ NA_character_,
          measurement_type == "recouvrement" ~ "%"
        )
      ) |>
      dplyr::filter(!is.na(measurement_value))


    # Biodiversity data integration
    # iterate over biodiversity_tables
    # Column mapping for standardization
    column_map <- list(
      event_id = c("event_id", "id"),
      scientific_name = c("scientific_name", "nom_scientifique"),
      scientific_name_id = c("scientific_name_id", "nom_scientifique_id"),
      scientific_name_id_db = c("scientific_name_id_db"),
      gbif_url = c("gbif_url"),
      taxon_rank = "taxon_rank",
      kingdom = "kingdom",
      phylum = "phylum",
      class = "class",
      order = "order",
      family = "family",
      genus = "genus",
      species = "species",
      measurement_type = c("measurement_type"),
      measurement_value = c("measurement_value"),
      measurement_unit = c("measurement_unit")
    )

    for (i in seq_len(length(biodiversity_tables))) {
      # Rename columns based on the mapping
      for (standard_col in names(column_map)) {
        matching_cols <- column_map[[standard_col]]
        existing_col <- intersect(matching_cols, colnames(biodiversity_tables[[i]]))

        if (length(existing_col) > 0) {
          biodiversity_tables[[i]] <- biodiversity_tables[[i]] |>
            dplyr::rename(!!standard_col := all_of(existing_col[1]))
        }
      }

      # Ensure required columns are present
      required_columns <- c("event_id")
      validate_columns(biodiversity_tables[[i]], required_columns)

      # Select only mapped columns and format dates
      biodiversity_tables[[i]] <- biodiversity_tables[[i]] |>
        dplyr::select(
          dplyr::all_of(intersect(names(column_map), colnames(biodiversity_tables[[i]])))
        ) |>
        dplyr::mutate(
          measurement_value = as.character(measurement_value),
          scientific_name_id = as.character(scientific_name_id)
        )
    }
    # Combine all event data into a single data frame
    biodiversity_tables <- dplyr::bind_rows(biodiversity_tables) |>
      dplyr::mutate(scientific_name = dplyr::if_else(
        !is.na(scientific_name),
        scientific_name,
        dplyr::case_when(
          !is.na(species) ~ species,
          !is.na(genus) ~ genus,
          !is.na(family) ~ family,
          !is.na(order) ~ order,
          !is.na(class) ~ class,
          !is.na(phylum) ~ phylum,
          !is.na(kingdom) ~ kingdom,
          .default = NA
        )
      ))

    # Adjust certain measurement units
    biodiversity_tables <- biodiversity_tables |>
      dplyr::mutate(
        measurement_unit = dplyr::case_when(
          measurement_type == "abundance" & (measurement_unit == "no unit" | is.na(measurement_unit)) ~ "n",
          measurement_type == "occurrence" ~ NA_character_,
          .default = measurement_unit
        )
      )

    return(biodiversity_tables)
  }

  # ----------------------------------------------------------------------
  # Abiotic tables
  make_abiotic_tables <- function() {
    abiotic_tables <- list()

    # Define patterns to identify event-related tables
    patterns <- c("abiotic", "habitat_data", "physico_chem")

    # Column mapping for standardization
    column_map <- list(
      event_id = c("event_id", "activite_id"),
      measurement_category = c("measurement_category", "type"),
      measurement_type = c("measurement_type", "variable"),
      measurement_type_description = c("measurement_type_description", "variable_description"),
      measurement_value = c("measurement_value", "value"),
      measurement_unit = c("measurement_unit", "unit"),
      measurement_method = c("measurement_method", "protocol"),
      measurement_remarks = c("measurement_remarks", "note")
    )

    # Iterate over input files and load data
    for (file in input_files) {
      # Extract database name and table name
      db_name <- stringr::str_extract(file, "(?<=harvested/|analyzed/)[^/]+")
      db_name <- stringr::str_remove(db_name, "-\\d+\\.\\d+\\.\\d+$") # Remove version
      table_name <- stringr::str_remove(basename(file), "\\.csv$") # Remove file extension


      # Check if the table name matches any of the event-related patterns
      if (any(stringr::str_detect(table_name, patterns))) {
        # Load data using vroom
        suppressWarnings({
          df <- vroom::vroom(file, progress = FALSE, show_col_types = FALSE, delim = ",")
        })

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
    return(abiotic_tables)
  }


  # Make tables
  event_tables <- make_events_tables()
  abiotic_tables <- make_abiotic_tables()
  biodiversity_tables <- make_biodiversity_tables()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ecosystems data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ecosystems <- input_files[grepl("resilience_cotiere_ecosystemes.gpkg", input_files)] |>
    sf::st_read(quiet = TRUE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Relational database tables
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Events table
  events <- event_tables |>
    dplyr::distinct() |>
    dplyr::mutate(
      event_date_start = as.character(event_date_start),
      event_date_end = as.character(event_date_end)
    )

  # ----------------------------------------------------------------------
  # Abiotic table
  abiotic <- abiotic_tables |>
    dplyr::distinct() |>
    dplyr::filter(event_id %in% events$event_id)

  # ----------------------------------------------------------------------
  # Taxonomy
  taxonomy <- biodiversity_tables |>
    dplyr::filter(event_id %in% events$event_id) |>
    dplyr::select(
      scientific_name, scientific_name_id, scientific_name_id_db, gbif_url,
      taxon_rank, kingdom, phylum, class, order, family, genus, species
    ) |>
    dplyr::distinct() |>
    # Some manual stuff...
    dplyr::mutate(
      scientific_name = stringr::str_replace_all(
        scientific_name,
        c("4" = NA_character_)
      )
    ) |>
    dplyr::group_by(scientific_name) |>
    dplyr::arrange(
      !is.na(scientific_name_id),
      rowSums(!is.na(dplyr::across(dplyr::everything()))),
      .by_group = TRUE
    ) |>
    dplyr::slice_tail(n = 1) |> # Keep the best row for each species
    dplyr::ungroup() |>
    dplyr::filter(!is.na(scientific_name)) |>
    dplyr::mutate(species_id = sprintf("sp_%06d", seq_len(dplyr::n()))) |>
    dplyr::relocate(species_id)


  # ----------------------------------------------------------------------
  # Biodiversity
  biodiversity <- biodiversity_tables |>
    dplyr::left_join(
      taxonomy |>
        dplyr::select(species_id, scientific_name),
      by = "scientific_name"
    ) |>
    dplyr::select(event_id, species_id, measurement_type, measurement_value, measurement_unit) |>
    dplyr::filter(event_id %in% events$event_id)

  # ----------------------------------------------------------------------
  # Metadata
  metadata <- event_tables |>
    dplyr::select(project_id, project_name, longitude, latitude, event_date_start, event_date_end) |>
    dplyr::mutate(project_id = tolower(project_id)) |>
    dplyr::group_by(project_id, project_name) |>
    dplyr::summarize(
      longitude_min = min(longitude, na.rm = TRUE),
      longitude_max = max(longitude, na.rm = TRUE),
      latitude_min = min(latitude, na.rm = TRUE),
      latitude_max = max(latitude, na.rm = TRUE),
      date_start = dplyr::if_else(
        all(is.na(event_date_start)),
        NA,
        min(event_date_start, na.rm = TRUE) |>
          lubridate::as_date() |>
          as.character()
      ),
      date_end = dplyr::if_else(
        all(is.na(event_date_end)),
        NA,
        min(event_date_end, na.rm = TRUE) |>
          lubridate::as_date() |>
          as.character()
      ),
      n_events = dplyr::n()
    )

  # ----------------------------------------------------------------------
  # Events - ecosystems
  event_ecosystem <- events |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(32198) |>
    dplyr::select(event_id) |>
    sf::st_join(ecosystems) |>
    sf::st_drop_geometry() |>
    dplyr::select(event_id, ecosystem_id)

  # ----------------------------------------------------------------------
  # Ecosystems
  ecosystems <- ecosystems |>
    sf::st_drop_geometry() |>
    dplyr::distinct() |>
    dplyr::arrange(ecosystem_id)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Relational database
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initiate sqlite
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(output_path, "biodiversity_data.sqlite"))

  # Add tables
  DBI::dbWriteTable(con, "events", events, overwrite = TRUE)
  DBI::dbWriteTable(con, "abiotic", abiotic, overwrite = TRUE)
  DBI::dbWriteTable(con, "biodiversity", biodiversity, overwrite = TRUE)
  DBI::dbWriteTable(con, "taxonomy", taxonomy, overwrite = TRUE)
  DBI::dbWriteTable(con, "metadata", metadata, overwrite = TRUE)
  DBI::dbWriteTable(con, "ecosystems", ecosystems, overwrite = TRUE)
  DBI::dbWriteTable(con, "event_ecosystem", event_ecosystem, overwrite = TRUE)

  # BD Schema
  dm::dm_from_con(con) |>
    dm::dm_add_pk(table = "events", "event_id") |>
    dm::dm_add_pk(table = "taxonomy", "species_id") |>
    dm::dm_add_pk(table = "metadata", "project_id") |>
    dm::dm_add_pk(table = "ecosystems", "ecosystem_id") |>
    dm::dm_add_fk(table = "abiotic", "event_id", "events") |>
    dm::dm_add_fk(table = "events", "project_id", "metadata") |>
    dm::dm_add_fk(table = "biodiversity", "event_id", "events") |>
    dm::dm_add_fk(table = "biodiversity", "species_id", "taxonomy") |>
    dm::dm_add_fk(table = "event_ecosystem", "event_id", "events") |>
    dm::dm_add_fk(table = "event_ecosystem", "ecosystem_id", "ecosystems") |>
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

get_taxonomy <- function(id, db) {
  if (!is.na(id) & !is.na(db)) {
    classification <- taxize::classification(id, db = db) # Use ID if available
  } else {
    classification <- list(NULL)
  }

  # Handle cases where no data is returned
  if (is.null(classification[[1]])) {
    return(data.frame(Kingdom = NA, Phylum = NA, Class = NA, Order = NA, Family = NA, Genus = NA, Species = NA))
  }

  # Extract relevant taxonomic ranks
  taxonomy <- classification[[1]] |>
    dplyr::select(-id) |>
    dplyr::filter(rank %in% c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) |>
    tidyr::pivot_wider(names_from = rank, values_from = name) # |>
  # dplyr::mutate(rank = dplyr::case_when(
  #   !is.na(Species) ~ "Species",
  #   !is.na(Genus) ~ "Genus",
  #   !is.na(Family) ~ "Family",
  #   !is.na(Order) ~ "Order",
  #   !is.na(Class) ~ "Class",
  #   !is.na(Phylum) ~ "Phylum",
  #   !is.na(Kingdom) ~ "Kingdom",
  #   .default = NA
  # ))
  return(taxonomy)
}
